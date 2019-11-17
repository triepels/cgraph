/*
Copyright 2019 Ron Triepels

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

  SEXP class_attrib = Rf_getAttrib(env, R_ClassSymbol);

  if(Rf_isNull(class_attrib))
  {
    return FALSE;
  }

  int n = XLENGTH(class_attrib);

  for(int i = 0; i < n; i++)
  {
    if(strcmp(CHAR(STRING_ELT(class_attrib, i)), class_name) == 0)
    {
      return TRUE;
    }
  }

  return FALSE;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_class1(const char *class_name1)
{
  SEXP env = PROTECT(Rf_allocSExp(ENVSXP));

  SET_FRAME(env, R_NilValue);
  SET_HASHTAB(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);

  Rf_setAttrib(env, R_ClassSymbol, Rf_mkString(class_name1));

  UNPROTECT(1);

  return env;
}

SEXP cg_class2(const char *class_name1, const char *class_name2)
{
  SEXP env = PROTECT(Rf_allocSExp(ENVSXP));

  SET_FRAME(env, R_NilValue);
  SET_HASHTAB(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);

  SEXP class_attrib = PROTECT(Rf_allocVector(STRSXP, 2));

  SET_STRING_ELT(class_attrib, 0, Rf_mkChar(class_name1));
  SET_STRING_ELT(class_attrib, 1, Rf_mkChar(class_name2));

  Rf_setAttrib(env, R_ClassSymbol, class_attrib);

  UNPROTECT(2);

  return env;
}
