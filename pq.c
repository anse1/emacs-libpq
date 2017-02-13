#include <emacs-module.h>
#include <libpq-fe.h>
#include "pg_type.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_PQ_PARAMS 12

int plugin_is_GPL_compatible;

static emacs_value Qnil;
static emacs_value Qt;

#define NOTICE_FORMAT "pq: %s"

static void pq_notice_rx (void *arg, const PGresult *res)
{
     const char *msg = PQresultErrorMessage(res);
     emacs_env *env = arg;
     emacs_value Fmessage = env->intern (env, "message");
     size_t len = strlen(msg);
     if (!len)
	  return;
     emacs_value args [2] = {
	  env->make_string(env, NOTICE_FORMAT, strlen(NOTICE_FORMAT)),
	  env->make_string(env, msg, len-1 /* cut trailing newline */)
     };
     env->funcall (env, Fmessage, 2, args);
}

static void pq_finalize_pointer(void *user_ptr)
{
  PGconn *conn = user_ptr;
  PQfinish(conn);
}

/* Raise error unless a PGresult is ok. */
static bool result_ok(emacs_env *env, PGresult *res)
{
  int status = PQresultStatus(res);
  switch (status) {
  case PGRES_NONFATAL_ERROR:
  case PGRES_TUPLES_OK:
  case PGRES_SINGLE_TUPLE:
  case PGRES_COMMAND_OK:
    return true;

  case PGRES_FATAL_ERROR:
  default:
    {
      const char *errmsg = PQresultErrorMessage(res);
      emacs_value errstring = env->make_string(env, errmsg, strlen(errmsg));
      emacs_value Qpq_error = env->intern (env, "error");

      PQclear(res);
      env->non_local_exit_signal(env, Qpq_error, errstring);
    }
    return false;
  }
}

static char *my_string_to_c(emacs_env *env, emacs_value string)
{
  ptrdiff_t size;
  emacs_value teststring = env->make_string(env, "", 0);

  if (!env->eq(env,
	       env->type_of(env, teststring),
	       env->type_of(env, string))) {
       emacs_value Fprin1_to_string =
	    env->intern(env, "prin1-to-string");
       string = env->funcall(env, Fprin1_to_string,
			     1, &string);
  }

  env->copy_string_contents(env, string, 0, &size);
  char *buf = malloc(size);
  env->copy_string_contents(env, string, buf, &size);
  return buf;
}

static emacs_value
Fpq_connectdb (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  char *conninfo = nargs ? my_string_to_c(env, args[0]) : "";
  PGconn *conn = PQconnectdb(conninfo);

  if (PQstatus(conn) != CONNECTION_OK) {
    const char *errmsg = PQerrorMessage(conn);
    emacs_value errstring = env->make_string(env, errmsg, strlen(errmsg));
    emacs_value Qpq_error = env->intern (env, "error");

    env->non_local_exit_signal(env, Qpq_error, errstring);
    if (nargs)
      free(conninfo);
    PQfinish(conn);
    return Qnil;
  }

  /* The emacs-module interface always expects utf8 strings */
  PGresult *res =
    PQexec(conn,
	   "set client_encoding to utf8;"
	   "set application_name to emacs;");

  if (!result_ok(env, res)) {
    if (nargs)
      free(conninfo);
    PQfinish(conn);
    return Qnil;
  }

  PQclear(res);
  if (nargs)
    free(conninfo);

  return env->make_user_ptr(env, pq_finalize_pointer, conn);
}

static emacs_value
pq_getvalue_internal(emacs_env *env, PGresult *res, int row, int column)
{
  if (PQgetisnull(res, row, column))
    return Qnil;

  char *result = PQgetvalue(res, row, column);
  if (!result)
    return Qnil;

  switch(PQftype(res, column)) {
  case BOOLOID:
    return ('t' == *result) ? Qt : Qnil;
  case INT2OID:
  case INT4OID:
  case OIDOID:
    return env->make_integer(env, atol(result));
  case INT8OID:
  case FLOAT4OID:
  case FLOAT8OID:
  case NUMERICOID:
    return env->make_float(env, atof(result));
  default:
    return env->make_string(env, result, strlen(result));
  }
}

static emacs_value
Fpq_query (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  PGconn *conn = env->get_user_ptr(env, args[0]);

  int nParams = nargs - 2;

  char *command = my_string_to_c(env, args[1]);

  PQnoticeReceiver old_notice_rx =
       PQsetNoticeReceiver(conn, pq_notice_rx, env);

  PGresult *res;

  if (nParams) {
       const char *paramValues[nParams];

       for (int i=0; i<nParams; i++)
	    paramValues[i] = my_string_to_c(env, args[2+i]);

       res = PQexecParams(conn, command, nParams,
				    NULL, paramValues, NULL, NULL, 0);

       for (int i=0; i<nParams; i++)
	    free((void *)paramValues[i]);
  } else
	 res = PQexec(conn, command);

  PQsetNoticeReceiver(conn, old_notice_rx, NULL);


  free(command);

  if (!result_ok(env, res))
    return Qnil;

  old_notice_rx =
       PQsetNoticeReceiver(conn, pq_notice_rx, env);

  int ntuples = PQntuples(res);
  int nfields = PQnfields(res);

  emacs_value list = Qnil;
  emacs_value Qvector = env->intern (env, "vector");
  emacs_value Qcons = env->intern (env, "cons");

  for (int t = ntuples-1; t >= 0; t--) {
    emacs_value tuple;
    if (1 == nfields) {
      tuple = pq_getvalue_internal(env, res, t, 0);
    } else if (0 == nfields) {
      tuple = Qnil;
    } else {
      emacs_value values[nfields+1];
      for (int i = 0; i < nfields; i++) {
	values[i] = pq_getvalue_internal(env, res, t, i);
      }
      values[nfields] = Qnil;
      tuple = env->funcall (env, Qvector, nfields, values);
    }

    emacs_value args[2] = {tuple, list};
    list = env->funcall (env, Qcons, 2, args);
  }

  PQsetNoticeReceiver(conn, old_notice_rx, NULL);
  PQclear(res);
  return list;
}

static emacs_value
Fpq_escape (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  PGconn *conn = env->get_user_ptr(env, args[0]);

  char *value = my_string_to_c(env, args[1]);
  char *(*escaper)(PGconn *, const char *, size_t) = data;
  char *quoted = escaper(conn, value, strlen(value));
  emacs_value result = env->make_string(env, quoted, strlen(quoted));
  PQfreemem(quoted);
  return result;
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'fset' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  /* Prepare the arguments array */
  emacs_value args[] = { Qsym, Sfun };

  /* Make the call (2 == nb of arguments) */
  env->funcall (env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
		 env->make_function (env, amin, amax, csym, doc, data))
  DEFUN("pq:connectdb", Fpq_connectdb, 0, 1,
	"Connect to PostgreSQL database described by CONNSTR.",
	NULL);

  DEFUN("pq:query", Fpq_query, 2, 2+MAX_PQ_PARAMS,
	"Execute QUERY on CONNECTION with optional PARAMETERS",
	NULL);

  DEFUN("pq:escapeLiteral", Fpq_escape, 2, 2,
	"Perform literal value quoting on STRING for CONN.",
	PQescapeLiteral);

  DEFUN("pq:escapeIdentifier", Fpq_escape, 2, 2,
	"Perform identifier value quoting on STRING for CONN.",
	PQescapeIdentifier);

#undef DEFUN

  Qnil = env->intern (env, "nil");
  Qt = env->intern (env, "t");

  provide(env, "pq");

  /* loaded successfully */
  return 0;
}
