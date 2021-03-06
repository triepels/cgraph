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
#include "initializer.h"

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

/*
 * LIBRARY INITIALIZATION
 */

static const R_CallMethodDef CallEntries[] = {
  // Node
  {"cg_constant",             (DL_FUNC) &cg_constant,             2},
  {"cg_parameter",            (DL_FUNC) &cg_parameter,            2},
  {"cg_input",                (DL_FUNC) &cg_input,                1},
  {"cg_operator",             (DL_FUNC) &cg_operator,             3},
  {"cg_node_print",           (DL_FUNC) &cg_node_print,           1},
  // Graph
  {"cg_graph",                (DL_FUNC) &cg_graph,                1},
  {"cg_graph_get",            (DL_FUNC) &cg_graph_get,            2},
  {"cg_graph_forward",        (DL_FUNC) &cg_graph_forward,        2},
  {"cg_graph_backward",       (DL_FUNC) &cg_graph_backward,       3},
  {"cg_graph_print",          (DL_FUNC) &cg_graph_print,          1},
  // Session
  {"cg_session_graph",        (DL_FUNC) &cg_session_graph,        0},
  {"cg_session_set_graph",    (DL_FUNC) &cg_session_set_graph,    1},
  // Function
  {"cg_function",             (DL_FUNC) &cg_function,             2},
  {"cg_function_print",       (DL_FUNC) &cg_function_print,       1},
  // Optimizer
  {"cg_optim_gd",             (DL_FUNC) &cg_optim_gd,             2},
  {"cg_optim_gd_momentum",    (DL_FUNC) &cg_optim_gd_momentum,    3},
  {"cg_optim_adagrad",        (DL_FUNC) &cg_optim_adagrad,        3},
  {"cg_optim_rmsprop",        (DL_FUNC) &cg_optim_rmsprop,        4},
  {"cg_optim_adam",           (DL_FUNC) &cg_optim_adam,           4},
  {"cg_optim_step",           (DL_FUNC) &cg_optim_step,           1},
  {"cg_optim_print",          (DL_FUNC) &cg_optim_print,          1},
  // Initializer
  {"cg_init_zeros",           (DL_FUNC) &cg_init_zeros,           2},
  {"cg_init_ones",            (DL_FUNC) &cg_init_ones,            2},
  {"cg_init_uniform",         (DL_FUNC) &cg_init_uniform,         4},
  {"cg_init_gaussian",        (DL_FUNC) &cg_init_gaussian,        4},
  {"cg_init_xavier_uniform",  (DL_FUNC) &cg_init_xavier_uniform,  2},
  {"cg_init_xavier_gaussian", (DL_FUNC) &cg_init_xavier_gaussian, 2},
  // Vector
  {"sigmoid",                 (DL_FUNC) &sigmoid,                 1},
  // Internal
  {"dots",                    (DL_FUNC) &dots,                    1},
  {"bsum",                    (DL_FUNC) &bsum,                    2},
  {"approx_gradient",         (DL_FUNC) &approx_gradient,         5},
  {NULL,                      NULL,                               0}
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
