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
#include <R_ext/Rdynload.h>

#include "node.h"
#include "class.h"
#include "graph.h"
#include "vector.h"
#include "session.h"
#include "symbols.h"
#include "function.h"
#include "internal.h"
#include "optimizer.h"

/*
 * SESSION DEFINITION
 */

cg_session_t session;

/*
 * SYMBOL DEFINITIONS
 */

SEXP CG_ID_SYMBOL       = NULL;
SEXP CG_DEF_SYMBOL      = NULL;
SEXP CG_EPS_SYMBOL      = NULL;
SEXP CG_ETA_SYMBOL      = NULL;
SEXP CG_FUN_SYMBOL      = NULL;
SEXP CG_OUT_SYMBOL      = NULL;
SEXP CG_GRAD_SYMBOL     = NULL;
SEXP CG_NAME_SYMBOL     = NULL;
SEXP CG_TYPE_SYMBOL     = NULL;
SEXP CG_BETAS_SYMBOL    = NULL;
SEXP CG_EAGER_SYMBOL    = NULL;
SEXP CG_GAMMA_SYMBOL    = NULL;
SEXP CG_GRADS_SYMBOL    = NULL;
SEXP CG_GRAPH_SYMBOL    = NULL;
SEXP CG_NODES_SYMBOL    = NULL;
SEXP CG_PARMS_SYMBOL    = NULL;
SEXP CG_VALUE_SYMBOL    = NULL;
SEXP CG_GAMMAS_SYMBOL   = NULL;
SEXP CG_INPUTS_SYMBOL   = NULL;
SEXP CG_BUFFER0_SYMBOL  = NULL;
SEXP CG_BUFFER1_SYMBOL  = NULL;

SEXP cg_vector_sin(SEXP a1, SEXP out)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  SEXP a0;

  R_len_t n = XLENGTH(a1);

  if(!Rf_isReal(out) || XLENGTH(out) != n)
  {
    PROTECT(a0 = Rf_allocVector(REALSXP, n));
  }
  else
  {
    PROTECT(a0 = out);
  }

  double *po = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
      {
        po[i] = sin(p1[i]);
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
      {
        po[i] = sin(p1[i]);
      }

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_vector_sin_grad(SEXP a1, SEXP grad, SEXP out)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a numeric vector or array");
  }

  R_len_t n = XLENGTH(a1);

  if(n != XLENGTH(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' and 'grad' have incompatible lengths");
  }

  SEXP a0;

  if(!Rf_isReal(out) || XLENGTH(out) != n)
  {
    PROTECT(a0 = Rf_allocVector(REALSXP, n));
  }
  else
  {
    PROTECT(a0 = out);
  }

  double *po = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
      {
        po[i] += pg[i] * cos(p1[i]);
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
      {
        po[i] += pg[i] * cos(p1[i]);
      }

      break;
    }
  }

  UNPROTECT(1);

  return a0;
}

/*
 * LIBRARY INITIALIZATION
 */

static const R_CallMethodDef CallEntries[] = {
  // Vector
  {"cg_vector_sin",         (DL_FUNC) &cg_vector_sin,         2},
  {"cg_vector_sin_grad",    (DL_FUNC) &cg_vector_sin_grad,    3},
  // Node
  {"cg_constant",           (DL_FUNC) &cg_constant,           2},
  {"cg_parameter",          (DL_FUNC) &cg_parameter,          2},
  {"cg_input",              (DL_FUNC) &cg_input,              1},
  {"cg_operator",           (DL_FUNC) &cg_operator,           3},
  // Graph
  {"cg_graph",              (DL_FUNC) &cg_graph,              1},
  {"cg_graph_get",          (DL_FUNC) &cg_graph_get,          2},
  {"cg_graph_forward",      (DL_FUNC) &cg_graph_forward,      2},
  {"cg_graph_backward",     (DL_FUNC) &cg_graph_backward,     3},
  // Function
  {"cg_function",           (DL_FUNC) &cg_function,           2},
  // Optimizer
  {"cg_gd",                 (DL_FUNC) &cg_gd,                 2},
  {"cg_gd_momentum",        (DL_FUNC) &cg_gd_momentum,        3},
  {"cg_adagrad",            (DL_FUNC) &cg_adagrad,            3},
  {"cg_rmsprop",            (DL_FUNC) &cg_rmsprop,            4},
  {"cg_adam",               (DL_FUNC) &cg_adam,               4},
  {"cg_optimizer_step",     (DL_FUNC) &cg_optimizer_step,     1},
  // Session
  {"cg_session_graph",      (DL_FUNC) &cg_session_graph,      0},
  {"cg_session_set_graph",  (DL_FUNC) &cg_session_set_graph,  1},
  // Internal
  {"dots",                  (DL_FUNC) &dots,                  1},
  {"bsum",                  (DL_FUNC) &bsum,                  2},
  {"approx_gradient",       (DL_FUNC) &approx_gradient,       5},
  // Vector
  {"sigmoid",               (DL_FUNC) &sigmoid,               1},
  {NULL,                    NULL,                             0}
};

void R_init_cgraph(DllInfo *dll)
{
  // Register c routines in R session
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // Install symbols
  CG_ID_SYMBOL        = Rf_install("id");
  CG_DEF_SYMBOL       = Rf_install("def");
  CG_EPS_SYMBOL       = Rf_install("eps");
  CG_ETA_SYMBOL       = Rf_install("eta");
  CG_FUN_SYMBOL       = Rf_install("fun");
  CG_OUT_SYMBOL       = Rf_install("out");
  CG_GRAD_SYMBOL      = Rf_install("grad");
  CG_NAME_SYMBOL      = Rf_install("name");
  CG_TYPE_SYMBOL      = Rf_install("type");
  CG_BETAS_SYMBOL     = Rf_install("betas");
  CG_EAGER_SYMBOL     = Rf_install("eager");
  CG_GAMMA_SYMBOL     = Rf_install("gamma");
  CG_GRADS_SYMBOL     = Rf_install("grads");
  CG_NODES_SYMBOL     = Rf_install("nodes");
  CG_PARMS_SYMBOL     = Rf_install("parms");
  CG_VALUE_SYMBOL     = Rf_install("value");
  CG_GAMMAS_SYMBOL    = Rf_install("gammas");
  CG_INPUTS_SYMBOL    = Rf_install("inputs");
  CG_BUFFER0_SYMBOL   = Rf_install("buffer0");
  CG_BUFFER1_SYMBOL   = Rf_install("buffer1");
}
