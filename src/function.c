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

#include "function.h"

/*
 * INLINED GET/SET FUNCTIONS
 */

extern inline SEXP cg_function_def(SEXP function);

extern inline void cg_function_set_def(SEXP function, SEXP def);

extern inline SEXP cg_function_grads(SEXP function);

extern inline void cg_function_set_grads(SEXP function, SEXP grads);

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_function_print(SEXP function)
{
  Rprintf("<cg_function>\n");

  return R_NilValue;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_function(SEXP def, SEXP grads)
{
  if(!Rf_isFunction(def))
  {
    Rf_errorcall(R_NilValue, "argument 'def' must be a function");
  }

  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'grads' must be a list of gradient functions");
  }

  R_len_t n = XLENGTH(grads);

  for(int i = 0; i < n; i++)
  {
    SEXP grad = VECTOR_ELT(grads, i);

    if(!Rf_isFunction(grad))
    {
      Rf_errorcall(R_NilValue, "invalid gradient provided to argument 'grads' at index %d", i + 1);
    }
  }

  SEXP function = PROTECT(cg_class("cg_function"));

  CG_SET(function, CG_GRADS_SYMBOL, grads);

  CG_SET(function, CG_DEF_SYMBOL, def);

  UNPROTECT(1);

  return function;
}
