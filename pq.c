#include <emacs-module.h>
#include <libpq-fe.h>
#include "pg_type.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_PQ_PARAMS 12

int plugin_is_GPL_compatible;

#define Qnil env->intern(env, "nil")
#define Qt env->intern(env, "t")

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
      const char *sqlstate = PQresultErrorField(res, PG_DIAG_SQLSTATE);
      emacs_value Qpq_error = env->intern (env, "pq:error");
      emacs_value errmsg_string =
	   env->make_string(env, errmsg, strlen(errmsg));
      emacs_value sqlstate_string =
	   env->make_string(env, sqlstate, strlen(sqlstate));
      emacs_value errdata[] = {errmsg_string, sqlstate_string};
      emacs_value errdata_list =
	   env->funcall(env, env->intern(env, "list"), 2, errdata);

      PQclear(res);
      env->non_local_exit_signal(env, Qpq_error, errdata_list);
    }
    return false;
  }
}

/* Raise error unless a PGConn is ok. */
static bool connection_ok(emacs_env *env, PGconn *conn)
{
  if (PQstatus(conn) != CONNECTION_OK) {
    const char *errmsg = PQerrorMessage(conn);
    emacs_value errstring = env->make_string(env, errmsg, strlen(errmsg));
    emacs_value Qpq_error = env->intern (env, "error");
    emacs_value errdata = env->funcall (env, env->intern(env, "list"), 1, &errstring);
    env->non_local_exit_signal(env, Qpq_error, errdata);
    return false;
  }
  return true;
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

  if (!connection_ok(env, conn)) {
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

  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
       return NULL;

  if (!connection_ok(env, conn)) {
       return Qnil;
  }

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

  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return NULL;

  char *value = my_string_to_c(env, args[1]);
  char *(*escaper)(PGconn *, const char *, size_t) = data;
  char *quoted = escaper(conn, value, strlen(value));
  emacs_value result = env->make_string(env, quoted, strlen(quoted));
  PQfreemem(quoted);
  return result;
}

static emacs_value
Fpq_reset (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  PGconn *conn = env->get_user_ptr(env, args[0]);

  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return NULL;

  PQreset(conn);

  if (!connection_ok(env, conn)) {
    return Qnil;
  }

  return Qt;
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
	"Connect to a PostgreSQL database described by CONNINFO.\n"
	"\n"
	"CONNINFO is a Connection String as defined in the PostgreSQL manual.\n"
	"E.g., \"host=localhost port=5432 dbname=mydb connect_timeout=10\"\n"
	"\n"
	"Return a user-ptr representing the libpq connection.\n"
	"Error if connection cannot established.\n"
	"\n\(fn CONNINFO)",
	NULL);

  DEFUN("pq:query", Fpq_query, 2, 2+MAX_PQ_PARAMS,
	"Execute COMMAND on CONN with optional PARAMETERS.\n"
	"\n"
	"Run an SQL command on a connection obtained by `pq:connectdb'.\n"
	"If PARAMETERS are used, you can reference them in COMMAND using\n"
	"$1, $2, ..., $12.\n"
	"\n"
	"Return the list of rows returned by the last statement in COMMAND.\n"
	"Rows are returned as atomic values if the statement yields a single\n"
	"column, or a vector of values if it yields more than one column.\n"
	"\n"
	"SQL values are turned into Lisp strings except for the following:\n"
	"\n"
	"    NULL: always returned as nil\n"
	"    true/false: t/nil\n"
	"    integer: integer or float, depending on size\n"
	"    float, numeric: float\n"
	"\n"
	"While these conversions are convenient, they may loose precision.\n"
	"If you need the full precision of big integer values or need to be\n"
	"able to discern between NULL and false, simply cast these to text\n"
	"in your query.\n"
	"\n"
	"Error on SQL errors.  Diagnostic information such as warnings is\n"
	"emitted using `message'.\n"
	"\n\(fn CONN COMMAND &rest PARAMETERS)"
	,
	NULL);

  DEFUN("pq:escapeLiteral", Fpq_escape, 2, 2,
	"Perform literal value quoting on STRING for CONN.\n"
	"\n"
	"Return a string for use within an SQL command.  This is useful\n"
	"when inserting data values as literal constants in SQL commands.\n"
	"Certain characters (such as quotes and backslashes) must be\n"
	"escaped to prevent them from being interpreted specially by the\n"
	"SQL parser.\n"
	"\n"
	"Note that it is not necessary nor correct to do escaping when a\n"
	"data value is passed as a separate parameter in `pq:query'.\n"
	"\n\(fn CONN STRING)",
	PQescapeLiteral);

  DEFUN("pq:escapeIdentifier", Fpq_escape, 2, 2,
	"Perform identifier value quoting on STRING for CONN.\n"
	"\n"
	"Return a string for use as an SQL identifier, such as a table,\n"
	"column, or function name.  This is useful when a user-supplied\n"
	"identifier might contain special characters that would otherwise\n"
	"not be interpreted as part of the identifier by the SQL parser,\n"
	"or when the identifier might contain upper case characters whose\n"
	"case should be preserved.\n"
	"\n\(fn CONN STRING)",
	PQescapeIdentifier);

  DEFUN("pq:reset", Fpq_reset, 1, 1,
	"Resets the communication channel to the server behind CONN.\n"
	"\n"
	"Return t if connection is ok again.\n"
	"\n\(fn CONN)",
	NULL);

#undef DEFUN

  /* Define custom error signal.  The error data is a list with two
   * strings.  The first string is the human-readable message, the
   * second is the SQLSTATE error code. */
  {
       emacs_value Fdefine_error = env->intern (env, "define-error");
       emacs_value Qpq_error = env->intern (env, "pq:error");
       emacs_value errmsg_string =
	    env->make_string(env, "SQL error", strlen("SQL error"));
       emacs_value args[] = {Qpq_error, errmsg_string};
       env->funcall(env, Fdefine_error, 2, args);
  }

  provide(env, "pq");

  /* loaded successfully */
  return 0;
}
