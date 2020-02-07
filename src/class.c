/*
Copyright 2020 Ron Triepels

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"

/*
 * PRIVATE METHODS
 */

int cg_is(SEXP env, const char *class_name)
{
  if(!Rf_isEnvironment(env))
  {
    return FALSE;
  }

  if(!Rf_inherits(env, class_name))
  {
    return FALSE;
  }

  return TRUE;
}

void cg_class_lock(SEXP env, const cg_class_def_t *def)
{
  int i = 0;

  while(def[i].name != NULL)
  {
    if(def[i].locked)
    {
      R_LockBinding(Rf_install(def[i].name), env);
    }

    i++;
  }

  if(!R_EnvironmentIsLocked(env))
  {
    R_LockEnvironment(env, FALSE);
  }
}

void cg_class_unlock(SEXP env, const cg_class_def_t *def)
{
  int i = 0;

  while(def[i].name != NULL)
  {
    if(def[i].locked)
    {
      R_unLockBinding(Rf_install(def[i].name), env);
    }

    i++;
  }
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_class(const char *name, const cg_class_def_t *def)
{
  SEXP env = PROTECT(Rf_allocSExp(ENVSXP));

  SET_FRAME(env, R_NilValue);
  SET_HASHTAB(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);

  int i = 0;

  while(def[i].name != NULL)
  {
    CG_SET(env, Rf_install(def[i].name), R_NilValue);

    i++;
  }

  Rf_setAttrib(env, R_ClassSymbol, Rf_mkString(name));

  UNPROTECT(1);

  return env;
}
