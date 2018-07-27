/*
Copyright (C) 2018 Ron Triepels

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <R.h>
#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP address(SEXP);
extern SEXP bsum(SEXP, SEXP);
extern SEXP cgraph(SEXP, SEXP);
extern SEXP cg_add_operation(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_add_parms();
extern SEXP cg_add_placeholder(SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_adj_mat(SEXP);
extern SEXP cg_approx_grad(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_gen_name(SEXP, SEXP);
extern SEXP cg_get_parms(SEXP);
extern SEXP cg_gradients(SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_run(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"address",            (DL_FUNC) &address,            1},
  {"bsum",               (DL_FUNC) &bsum,               2},
  {"cgraph",             (DL_FUNC) &cgraph,             2},
  {"cg_add_operation",   (DL_FUNC) &cg_add_operation,   5},
  {"cg_add_parms",       (DL_FUNC) &cg_add_parms,       2},
  {"cg_add_placeholder", (DL_FUNC) &cg_add_placeholder, 4},
  {"cg_adj_mat",         (DL_FUNC) &cg_adj_mat,         1},
  {"cg_approx_grad",     (DL_FUNC) &cg_approx_grad,     6},
  {"cg_gen_name",        (DL_FUNC) &cg_gen_name,        2},
  {"cg_get_parms",       (DL_FUNC) &cg_get_parms,       1},
  {"cg_gradients",       (DL_FUNC) &cg_gradients,       4},
  {"cg_run",             (DL_FUNC) &cg_run,             3},
  {NULL, NULL, 0}
};

void R_init_cgraph(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
