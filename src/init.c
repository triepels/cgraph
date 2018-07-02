#include <R.h>
#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP bsum(SEXP, SEXP);
extern SEXP cg_add_expression(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_add_placeholder(SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_adj_mat(SEXP);
extern SEXP cg_approx_grad;
extern SEXP cg_gen_name(SEXP, SEXP);
extern SEXP cg_gradients(SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_run(SEXP, SEXP, SEXP);
extern SEXP cg_set(SEXP, SEXP, SEXP);
extern SEXP cg_types();
extern SEXP cgraph(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"bsum",               (DL_FUNC) &bsum,               2},
  {"cg_add_expression",  (DL_FUNC) &cg_add_expression,  5},
  {"cg_add_placeholder", (DL_FUNC) &cg_add_placeholder, 4},
  {"cg_adj_mat",         (DL_FUNC) &cg_adj_mat,         1},
  {"cg_approx_grad",     (DL_FUNC) &cg_approx_grad,     6},
  {"cg_gen_name",        (DL_FUNC) &cg_gen_name,        2},
  {"cg_gradients",       (DL_FUNC) &cg_gradients,       4},
  {"cg_run",             (DL_FUNC) &cg_run,             3},
  {"cg_set",             (DL_FUNC) &cg_set,             3},
  {"cg_types",           (DL_FUNC) &cg_types,           0},
  {"cgraph",             (DL_FUNC) &cgraph,             1},
  {NULL, NULL, 0}
};

void R_init_cgraph(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
