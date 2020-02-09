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

#ifndef FUNCTION_H
#define FUNCTION_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"
#include "symbols.h"

/*
 * INLINED GET/SET FUNCTIONS
 */

inline SEXP cg_function_def(SEXP function)
{
    SEXP def = PROTECT(CG_GET(function, CG_DEF_SYMBOL));

    if(!Rf_isFunction(def))
    {
        Rf_errorcall(R_NilValue, "function has no definition");
    }

    UNPROTECT(1);

    return def;
}

inline void cg_function_set_def(SEXP function, SEXP def)
{
    if(!Rf_isFunction(def))
    {
        Rf_errorcall(R_NilValue, "argument 'def' must be a function");
    }

    CG_SET(function, CG_DEF_SYMBOL, def);
}

inline SEXP cg_function_grads(SEXP function)
{
    SEXP grads = PROTECT(CG_GET(function, CG_GRADS_SYMBOL));

    if(TYPEOF(grads) != VECSXP)
    {
        Rf_errorcall(R_NilValue, "function has no gradients");
    }

    UNPROTECT(1);

    return grads;
}

inline void cg_function_set_grads(SEXP function, SEXP grads)
{
    if(TYPEOF(grads) != VECSXP)
    {
        Rf_errorcall(R_NilValue, "argument 'grads' must be a list of gradient functions");
    }

    CG_SET(function, CG_GRADS_SYMBOL, grads);
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_function(SEXP def, SEXP grads);

#endif
