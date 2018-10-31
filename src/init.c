/*
Copyright 2018 Ron Triepels
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

#include <R.h>
#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// cgraph.c functions
extern SEXP cgraph(SEXP, SEXP, SEXP);
extern SEXP cg_add_constant(SEXP, SEXP, SEXP);
extern SEXP cg_add_input(SEXP, SEXP, SEXP);
extern SEXP cg_add_operation(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_add_parameter(SEXP, SEXP, SEXP);
extern SEXP cg_add_parms();
extern SEXP cg_adj_mat(SEXP);
extern SEXP cg_get(SEXP, SEXP);
extern SEXP cg_get_parms(SEXP);
extern SEXP cg_gradients(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_run(SEXP, SEXP, SEXP);
extern SEXP cg_set(SEXP, SEXP, SEXP);

// internal.c functions
extern SEXP bsum(SEXP, SEXP);

// misc.c functions
extern SEXP sigmoid(SEXP);

static const R_CallMethodDef CallEntries[] = {
  // cgraph.c functions
  {"cgraph",             (DL_FUNC) &cgraph,             3},
  {"cg_add_constant",    (DL_FUNC) &cg_add_constant,    3},
  {"cg_add_input",       (DL_FUNC) &cg_add_input,       3},
  {"cg_add_operation",   (DL_FUNC) &cg_add_operation,   5},
  {"cg_add_parameter",   (DL_FUNC) &cg_add_parameter,   3},
  {"cg_add_parms",       (DL_FUNC) &cg_add_parms,       2},
  {"cg_adj_mat",         (DL_FUNC) &cg_adj_mat,         1},
  {"cg_get",             (DL_FUNC) &cg_get,             2},
  {"cg_get_parms",       (DL_FUNC) &cg_get_parms,       1},
  {"cg_gradients",       (DL_FUNC) &cg_gradients,       5},
  {"cg_run",             (DL_FUNC) &cg_run,             3},
  {"cg_set",             (DL_FUNC) &cg_set,             3},
  // internal.c functions
  {"bsum",               (DL_FUNC) &bsum,               2},
  // misc.c functions
  {"sigmoid",            (DL_FUNC) &sigmoid,            1},

  {NULL, NULL, 0}
};

void R_init_cgraph(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
