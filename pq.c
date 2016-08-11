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

/* We pass different kinds of libpq pointers to lisp.  We wrap them up
   with type information so we don't crash if the user mixes them up. */
struct pq_pointer {
  enum pq_pointer_type {
    type_conn,
    type_res
  } type;
  union {
    PGconn *conn;
    PGresult *res;
  } p;
};

void pq_finalize_pointer(void *user_ptr)
{
  struct pq_pointer *ptr = user_ptr;

  /* Do libpq cleanup */
  switch (ptr->type) {
  case type_conn: {
    PQfinish(ptr->p.conn);
    fprintf(stderr, "PQfinish(%p)\n", ptr->p.conn);
    break;
  }
  case type_res:
    fprintf(stderr, "PQclear(%p)\n", ptr->p.res);
    PQclear(ptr->p.res);
    break;
  }

  /* Free our wrapper */
  free(user_ptr);
}

/* Raise error unless a PGresult is ok. */
bool result_ok(emacs_env *env, PGresult *res)
{
  int status = PQresultStatus(res);
  switch (status) {
  case PGRES_TUPLES_OK:
  case PGRES_SINGLE_TUPLE:
  case PGRES_COMMAND_OK:
    return true;

  case PGRES_FATAL_ERROR:
  default:
    {
      char *errmsg = PQresultErrorMessage(res);
/*       char *errmsg = PQresStatus(status); */
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
  char *conninfo = my_string_to_c(env, args[0]);
  PGconn *conn = PQconnectdb(conninfo);

  char *errmsg = PQerrorMessage(conn);
  if (strlen(errmsg)) {
    emacs_value errstring = env->make_string(env, errmsg, strlen(errmsg));

    env->non_local_exit_signal(env, Qpq_error, errstring);
    free(conninfo);
    PQfinish(conn);
    return Qnil;
  }
  fprintf(stderr, "PQconnectdb(%s) -> %p\n", conninfo, conn);
  free(conninfo);

  struct pq_pointer *p = malloc(sizeof(struct pq_pointer));
  p->type = type_conn;
  p->p.conn = conn;
  return env->make_user_ptr(env, pq_finalize_pointer, p);
}

static emacs_value
Fpq_exec (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;

  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_conn == arg0->type);

  char *command = my_string_to_c(env, args[1]);
  PGresult *res = PQexec(arg0->p.conn, command);
  free(command);

  if (!result_ok(env, res))
    return Qnil;

  struct pq_pointer *p = malloc(sizeof(struct pq_pointer));
  p->type = type_res;
  p->p.res = res;
  return env->make_user_ptr(env, pq_finalize_pointer, p);
}

static emacs_value
Fpq_execParams (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;

  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_conn == arg0->type);

  int nParams = nargs - 2;
  const char *paramValues[nParams];

  for (int i=0; i<nParams; i++)
    paramValues[i] = my_string_to_c(env, args[2+i]);

  char *command = my_string_to_c(env, args[1]);
  PGresult *res = PQexecParams(arg0->p.conn, command, nParams,
			       NULL, paramValues, NULL, NULL, 0);

  for (int i=0; i<nParams; i++)
    free((void *)paramValues[i]);

  free(command);

  if (!result_ok(env, res))
    return Qnil;

  struct pq_pointer *p = malloc(sizeof(struct pq_pointer));
  p->type = type_res;
  p->p.res = res;
  return env->make_user_ptr(env, pq_finalize_pointer, p);
}

static emacs_value
Fpq_prepare (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;

  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_conn == arg0->type);

  char *name = my_string_to_c(env, args[1]);
  char *command = my_string_to_c(env, args[2]);

  PGresult *res = PQprepare(arg0->p.conn, name, command, 0, 0);

  free(name);
  free(command);

  if (!result_ok(env, res))
    return Qnil;

  return args[1];
}

static emacs_value
Fpq_execPrepared (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_conn == arg0->type);

  char *name = my_string_to_c(env, args[1]);

  int nParams = nargs - 2;
  const char *paramValues[nParams];
  for (int i=0; i<nParams; i++)
    paramValues[i] = my_string_to_c(env, args[2+i]);

  PGresult *res = PQexecParams(arg0->p.conn, name, nParams,
			       NULL, paramValues, NULL, NULL, 0);

  for (int i=0; i<nParams; i++)
    free((void *)paramValues[i]);

  if (!result_ok(env, res))
    return Qnil;

  struct pq_pointer *p = malloc(sizeof(struct pq_pointer));
  p->type = type_res;
  p->p.res = res;
  return env->make_user_ptr(env, pq_finalize_pointer, p);
}

static emacs_value
Fpq_ntuples (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_res == arg0->type);
  return env->make_integer(env, PQntuples(arg0->p.res));
}

static emacs_value
Fpq_nfields (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_res == arg0->type);
  return env->make_integer(env, PQnfields(arg0->p.res));
}

static emacs_value
Fpq_fname (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  int column = env->extract_integer(env, args[1]);
  assert(type_res == arg0->type);
  char *name = PQfname(arg0->p.res, column);
  return env->make_string(env, name, strlen(name));
}

static emacs_value
pq_getvalue_internal(emacs_env *env, PGresult *res, int row, int column)
{
  char *result = PQgetvalue(res, row, column);

  switch(PQftype(res, column)) {
  case INT2OID:
  case INT4OID:
      return env->make_integer(env, atol(result));
  case FLOAT4OID:
  case FLOAT8OID:
  case NUMERICOID:
    return env->make_float(env, atof(result));
  default:
    return env->make_string(env, result, strlen(result));
  }
}

static emacs_value
Fpq_getvalue(emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  int row = env->extract_integer(env, args[1]);
  int column = env->extract_integer(env, args[2]);
  assert(type_res == arg0->type);
  if (PQgetisnull(arg0->p.res, row, column))
    return Qnil;
  return pq_getvalue_internal(env, arg0->p.res, row, column);
}

/* static emacs_value */
/* Fpq_getrow(emacs_env *env, int nargs, emacs_value args[], void *data) */
/* { */
/*   struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]); */
/*   int row = env->extract_integer(env, args[1]); */
/*   assert(type_res == arg0->type); */
/*   int nfields = PQnfields(arg0->p.res); */
/*   emacs_value *values = malloc(nfields*sizeof(emacs_value)); */

/*   for (int i = 0; i < nfields; i++) { */
/*     values[i] = pq_getvalue_internal(env, arg0->p.res, row, i); */
/*   } */

/*   emacs_value Qvector = env->intern (env, "vector"); */
/*   return env->funcall (env, Qvector, nfields, values); */
/* } */

static emacs_value
Fpq_escape (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (!env->is_not_nil(env, args[0]))
    return Qnil;
  struct pq_pointer *arg0 = env->get_user_ptr(env, args[0]);
  assert(type_conn == arg0->type);

  char *value = my_string_to_c(env, args[1]);
  char *(*escaper)(PGconn *, const char *, size_t) = data;
  char *quoted = escaper(arg0->p.conn, value, strlen(value));
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
              1,            /* min. number of arguments */
              1,            /* max. number of arguments */
              Fpq_connectdb,  /* actual function pointer */
              "Connect to PostgreSQL database described by CONNSTR.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:connectdb", fun1);

  emacs_value fun2 = env->make_function (env,
              2,            /* min. number of arguments */
              2,            /* max. number of arguments */
              Fpq_exec,  /* actual function pointer */
              "Execute STATEMENT using CONNECTION.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:exec", fun2);

  emacs_value fun3 = env->make_function (env,
              1,            /* min. number of arguments */
              1,            /* max. number of arguments */
              Fpq_ntuples,  /* actual function pointer */
              "Return number of rows in result.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:ntuples", fun3);

  emacs_value fun4 = env->make_function (env,
              1,            /* min. number of arguments */
              1,            /* max. number of arguments */
              Fpq_nfields,  /* actual function pointer */
              "Return number of fields in rows of result.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:nfields", fun4);

  emacs_value fun5 = env->make_function (env,
              2,            /* min. number of arguments */
              2,            /* max. number of arguments */
              Fpq_fname,  /* actual function pointer */
              "Return name in RESULT of column NUMBER.",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:fname", fun5);

  emacs_value fun6 = env->make_function (env,
              3,            /* min. number of arguments */
              3,            /* max. number of arguments */
              Fpq_getvalue,  /* actual function pointer */
              "Return value for RESULT at ROW and COLUMN",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:getvalue", fun6);

  emacs_value fun7 = env->make_function (env,
              2,            /* min. number of arguments */
              2+MAX_PARAMS,  /* max. number of arguments */
              Fpq_execParams,  /* actual function pointer */
              "Return value for RESULT at ROW and COLUMN",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:execParams", fun7);

  emacs_value fun8 = env->make_function (env,
              3,            /* min. number of arguments */
              3,  /* max. number of arguments */
              Fpq_prepare,  /* actual function pointer */
              "Prepare statement NAME with STATEMENT ",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:prepare", fun8);

  emacs_value fun9 = env->make_function (env,
              2,            /* min. number of arguments */
              2+MAX_PARAMS,  /* max. number of arguments */
              Fpq_execPrepared,  /* actual function pointer */
              "Execute prepared statement NAME with ARGS...",        /* docstring */
              NULL          /* user pointer of your choice (data param in Fmymod_test) */
  );
  bind_function("pq:execPrepared", fun9);

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

/*   emacs_value fun12 = env->make_function (env, */
/*               2,            /\* min. number of arguments *\/ */
/*               2,  /\* max. number of arguments *\/ */
/*               Fpq_getrow,  /\* actual function pointer *\/ */
/*               "Fetch ROW from RESULT as a vector.",        /\* docstring *\/ */
/*               PQescapeIdentifier  /\* user pointer of your choice (data param in Fmymod_test) *\/ */
/*   ); */
/*   bind_function("pq:getrow", fun12); */

  Qnil = env->intern (env, "nil");
  Qt = env->intern (env, "t");
  Qpq_error = env->intern (env, "pq:error");

  provide("pq");

  /* loaded successfully */
  return 0;
}
