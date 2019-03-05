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
#include "function.h"

/*
 * SYMBOLS
 */

#define CG_DEF_SYMBOL Rf_install("def")
#define CG_GRADS_SYMBOL Rf_install("grads")

/*
 * PRIVATE METHODS
 */

SEXP cg_function_def(SEXP function)
{
  SEXP def = Rf_findVarInFrame(function, CG_DEF_SYMBOL);

  if(!Rf_isFunction(def))
  {
    Rf_errorcall(R_NilValue, "function has no definition");
  }

  return def;
}

void cg_function_set_def(SEXP function, SEXP def)
{
  if(!Rf_isFunction(def))
  {
    Rf_errorcall(R_NilValue, "argument 'def' must be a function");
  }

  Rf_defineVar(CG_DEF_SYMBOL, def, function);
}

SEXP cg_function_grads(SEXP function)
{
  SEXP grads = Rf_findVarInFrame(function, CG_GRADS_SYMBOL);

  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "function has no gradients");
  }

  return grads;
}

void cg_function_set_grads(SEXP function, SEXP grads)
{
  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'grads' must be a list of gradient functions");
  }

  R_len_t n = Rf_xlength(grads);

  for(int i = 0; i < n; i++)
  {
    SEXP grad = VECTOR_ELT(grads, i);

    if(!Rf_isFunction(grad))
    {
      Rf_errorcall(R_NilValue, "invalid gradient provided to argument 'grads' at index %d", i + 1);
    }
  }

  Rf_defineVar(CG_GRADS_SYMBOL, grads, function);
}

void cg_function_add_grad(SEXP function, SEXP grad)
{
  int index;

  SEXP grads = R_NilValue;

  PROTECT_WITH_INDEX(grads = Rf_findVarInFrame(function, CG_DEF_SYMBOL), &index);

  if(TYPEOF(grads) != VECSXP)
  {
    REPROTECT(grads = Rf_allocVector(VECSXP, 1), index);

    SET_VECTOR_ELT(grads, 0, grad);
  }
  else
  {
    R_len_t n = Rf_xlength(grads);

    REPROTECT(grads = Rf_lengthgets(grads, n + 1), index);

    SET_VECTOR_ELT(grads, n, grad);
  }

  Rf_defineVar(CG_GRADS_SYMBOL, grads, function);

  UNPROTECT(1);
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_function(SEXP def, SEXP grads)
{
  SEXP function = PROTECT(cg_class1("cg_function"));

  cg_function_set_def(function, def);
  cg_function_set_grads(function, grads);

  UNPROTECT(1);

  return function;
}
