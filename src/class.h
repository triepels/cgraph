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

#ifndef CLASS_H
#define CLASS_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * MACROS
 */

#define CG_GET(x, SYMBOL) Rf_findVarInFrame(x, SYMBOL)
#define CG_SET(x, SYMBOL, v) Rf_defineVar(SYMBOL, v, x)

/*
 * INLINED FUNCTIONS
 */

inline int cg_is(SEXP env, const char *name)
{
    if(!Rf_isEnvironment(env))
    {
        return FALSE;
    }

    if(!Rf_inherits(env, name))
    {
        return FALSE;
    }

    return TRUE;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_class(const char *name);

#endif
