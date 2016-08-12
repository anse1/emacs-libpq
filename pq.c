#include <emacs-module.h>
#include <libpq-fe.h>
#include "pg_type.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_PARAMS 12

void *plugin_is_GPL_compatible;

static struct emacs_runtime *ert;
static emacs_env *env;

static emacs_value Qnil;
static emacs_value Qt;
static emacs_value Qpq_error;

void pq_finalize_pointer(void *user_ptr)
{
  PGconn *conn = user_ptr;
  fprintf(stderr, "PQfinish(%p)\n", conn);
  PQfinish(conn);
}

/* Raise error unless a PGresult is ok. */
bool result_ok(emacs_env *env, PGresult *res)
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
      char *errmsg = PQresultErrorMessage(res);
      emacs_value errstring = env->make_string(env, errmsg, strlen(errmsg));

      PQclear(res);
      env->non_local_exit_signal(env, Qpq_error, errstring);
    }
    return false;
  }
}

static char *my_string_to_c(emacs_env *env, emacs_value string)
{
  ptrdiff_t size;
  env->copy_string_contents(env, string, 0, &size);
  char *buf = malloc(size);
  env->copy_string_contents(env, string, buf, &size);
  return buf;
}

static emacs_value
Fpq_connectdb (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  char *conninfo = nargs ? my_string_to_c(env, args[0]) : "";
  PGconn *conn = PQconnectdb(conninfo);

  char *errmsg = PQerrorMessage(conn);
  if (strlen(errmsg)) {
    emacs_value errstring = env->make_string(env, errmsg, strlen(errmsg));

    env->non_local_exit_signal(env, Qpq_error, errstring);
    if (nargs)
      free(conninfo);
    PQfinish(conn);
    return Qnil;
  }
  fprintf(stderr, "PQconnectdb(%s) -> %p\n", conninfo, conn);

  /* The emacs-module interface always expects utf8 strings */
  PGresult *res = PQexec(conn, "set client_encoding to utf8");
  if (!result_ok(env, res))
    return Qnil;

  if (nargs)
    free(conninfo);

  return env->make_user_ptr(env, pq_finalize_pointer, conn);
}

static emacs_value
pq_getvalue_internal(emacs_env *env, PGresult *res, int row, int column)
{
  char *result = PQgetvalue(res, row, column);

  switch(PQftype(res, column)) {
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
Fpq_query (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  PGconn *conn = env->get_user_ptr(env, args[0]);

  int nParams = nargs - 2;
  const char *paramValues[nParams];

  for (int i=0; i<nParams; i++)
    paramValues[i] = my_string_to_c(env, args[2+i]);

  char *command = my_string_to_c(env, args[1]);
  PGresult *res = PQexecParams(conn, command, nParams,
			       NULL, paramValues, NULL, NULL, 0);

  for (int i=0; i<nParams; i++)
    free((void *)paramValues[i]);

  free(command);

  if (!result_ok(env, res))
    return Qnil;

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
      emacs_value *values = malloc((nfields + 1)*sizeof(emacs_value));
      for (int i = 0; i < nfields; i++) {
	values[i] = pq_getvalue_internal(env, res, t, i);
      }
      values[nfields] = Qnil;
      tuple = env->funcall (env, Qvector, nfields, values);
    }

    emacs_value args[2] = {tuple, list};
    list = env->funcall (env, Qcons, 2, args);
  }

  return list;
}

static emacs_value
Fpq_escape (emacs_env *env, int nargs, emacs_value args[], void *data)
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
bind_function (const char *name, emacs_value Sfun)
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
provide (const char *feature)
{
  /* call 'provide' with FEATURE converted to a symbol */

  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *init_ert)
{
  ert = init_ert;
  env = ert->get_environment(ert);

  emacs_value fun1 = env->make_function (env,
              0,            /* min. number of arguments */
              1,            /* max. number of arguments */
              Fpq_connectdb,  /* actual function pointer */
              "Connect to PostgreSQL database described by CONNSTR.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:connectdb", fun1);

  emacs_value fun7 = env->make_function (env,
              2,            /* min. number of arguments */
              2+MAX_PARAMS,  /* max. number of arguments */
              Fpq_query,  /* actual function pointer */
              "Execute QUERY on CONNECTION.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:query", fun7);

    emacs_value fun10 = env->make_function (env,
              2,            /* min. number of arguments */
              2+MAX_PARAMS,  /* max. number of arguments */
              Fpq_escape,  /* actual function pointer */
              "Perform literal value quoting on STRING for CONN.",        /* docstring */
              PQescapeLiteral /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:escapeLiteral", fun10);

  emacs_value fun11 = env->make_function (env,
              2,            /* min. number of arguments */
              2+MAX_PARAMS,  /* max. number of arguments */
              Fpq_escape,  /* actual function pointer */
              "Perform identifier quoting on STRING for CONN.",        /* docstring */
              PQescapeIdentifier  /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:escapeIdentifier", fun11);

  Qnil = env->intern (env, "nil");
  Qt = env->intern (env, "t");
  Qpq_error = env->intern (env, "pq:error");

  provide("pq");

  /* loaded successfully */
  return 0;
}
