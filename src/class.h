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
 * SYMBOL DECLARATIONS
 */

extern SEXP CG_ID_SYMBOL;
extern SEXP CG_DEF_SYMBOL;
extern SEXP CG_FUN_SYMBOL;
extern SEXP CG_GRAD_SYMBOL;
extern SEXP CG_NAME_SYMBOL;
extern SEXP CG_TYPE_SYMBOL;
extern SEXP CG_EAGER_SYMBOL;
extern SEXP CG_GRADS_SYMBOL;
extern SEXP CG_NODES_SYMBOL;
extern SEXP CG_VALUE_SYMBOL;
extern SEXP CG_INPUTS_SYMBOL;

/*
 * PRIVATE METHODS
 */

int cg_is(SEXP env, const char *class_name);

/*
 * PRIVATE CONSTRUCTORS
 */

SEXP cg_class1(const char *class_name1);

SEXP cg_class2(const char *class_name1, const char *class_name2);

#endif
