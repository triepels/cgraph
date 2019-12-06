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
#include <R_ext/Rdynload.h>

#include "math.h"
#include "node.h"
#include "class.h"
#include "graph.h"
#include "session.h"
#include "function.h"
#include "internal.h"

/*
 * LIBRARY INITIALIZATION
 */

static const R_CallMethodDef CallEntries[] = {
  // Node
  {"cg_constant", (DL_FUNC) &cg_constant, 2},
  {"cg_parameter", (DL_FUNC) &cg_parameter, 2},
  {"cg_input", (DL_FUNC) &cg_input, 1},
  {"cg_operator", (DL_FUNC) &cg_operator, 3},
  // Graph
  {"cg_graph", (DL_FUNC) &cg_graph, 0},
  {"cg_graph_forward", (DL_FUNC) &cg_graph_forward, 2},
  {"cg_graph_backward", (DL_FUNC) &cg_graph_backward, 3},
  {"cg_graph_run", (DL_FUNC) &cg_graph_run, 3}, /* NOTE: DEPRECATED */
  {"cg_graph_gradients", (DL_FUNC) &cg_graph_gradients, 5}, /* NOTE: DEPRECATED */
  // Function
  {"cg_function", (DL_FUNC) &cg_function, 2},
  // Session
  {"cg_session_graph", (DL_FUNC) &cg_session_graph, 0},
  {"cg_session_set_graph", (DL_FUNC) &cg_session_set_graph, 1},
  // Internal
  {"bsum", (DL_FUNC) &bsum, 2},
  {"approx_gradient", (DL_FUNC) &approx_gradient, 5},
  // Math
  {"cg_math_pos", (DL_FUNC) &cg_math_pos, 1},
  {"cg_math_pos_grad", (DL_FUNC) &cg_math_pos_grad, 1},
  {"cg_math_neg", (DL_FUNC) &cg_math_neg, 1},
  {"cg_math_neg_grad", (DL_FUNC) &cg_math_neg_grad, 1},
  {"cg_math_add", (DL_FUNC) &cg_math_add, 2},
  {"cg_math_add_grad", (DL_FUNC) &cg_math_add_grad, 2},
  {"cg_math_sub", (DL_FUNC) &cg_math_add, 2},
  {"cg_math_sub_grad", (DL_FUNC) &cg_math_add_grad, 2},
  {"cg_math_mul", (DL_FUNC) &cg_math_mul, 2},
  {"cg_math_mul_grad1", (DL_FUNC) &cg_math_mul_grad1, 3},
  {"cg_math_mul_grad2", (DL_FUNC) &cg_math_mul_grad2, 3},
  {"cg_math_div", (DL_FUNC) &cg_math_div, 2},
  {"cg_math_div_grad1", (DL_FUNC) &cg_math_div_grad1, 3},
  {"cg_math_div_grad2", (DL_FUNC) &cg_math_div_grad2, 3},
  {"cg_math_pow", (DL_FUNC) &cg_math_pow, 2},
  {"cg_math_pow_grad1", (DL_FUNC) &cg_math_pow_grad1, 3},
  {"cg_math_pow_grad2", (DL_FUNC) &cg_math_pow_grad2, 3},
  {"cg_math_square", (DL_FUNC) &cg_math_square, 1},
  {"cg_math_square_grad", (DL_FUNC) &cg_math_square_grad, 2},
  {"cg_math_sqrt", (DL_FUNC) &cg_math_sqrt, 1},
  {"math_sin", (DL_FUNC) &math_sin, 1},
  {"sigmoid", (DL_FUNC) &sigmoid, 1},
  {"sigmoid_grad", (DL_FUNC) &sigmoid_grad, 3},
  {NULL, NULL, 0}
};

void R_init_cgraph(DllInfo *dll)
{
  // Register c routines in R session
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // Initialize a new session
  cg_session();
}
