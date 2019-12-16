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
  {"cg_math_pos_grad", (DL_FUNC) &cg_math_pos_grad, 2},
  {"cg_math_neg", (DL_FUNC) &cg_math_neg, 1},
  {"cg_math_neg_grad", (DL_FUNC) &cg_math_neg_grad, 2},
  {"cg_math_add", (DL_FUNC) &cg_math_add, 2},
  {"cg_math_add_grad", (DL_FUNC) &cg_math_add_grad, 2},
  {"cg_math_sub", (DL_FUNC) &cg_math_sub, 2},
  {"cg_math_sub_grad", (DL_FUNC) &cg_math_sub_grad, 2},
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
  {"cg_math_sqrt_grad", (DL_FUNC) &cg_math_sqrt_grad, 3},
  {"cg_math_cbrt", (DL_FUNC) &cg_math_cbrt, 1},
  {"cg_math_cbrt_grad", (DL_FUNC) &cg_math_cbrt_grad, 3},
  {"cg_math_hypot", (DL_FUNC) &cg_math_hypot, 2},
  {"cg_math_hypot_grad", (DL_FUNC) &cg_math_hypot_grad, 3},
  {"cg_math_fma", (DL_FUNC) &cg_math_fma, 3},
  {"cg_math_exp", (DL_FUNC) &cg_math_exp, 1},
  {"cg_math_exp_grad", (DL_FUNC) &cg_math_exp_grad, 3},
  {"cg_math_exp2", (DL_FUNC) &cg_math_exp2, 1},
  {"cg_math_exp2_grad", (DL_FUNC) &cg_math_exp2_grad, 3},
  {"cg_math_ln", (DL_FUNC) &cg_math_ln, 1},
  {"cg_math_ln_grad", (DL_FUNC) &cg_math_ln_grad, 2},
  {"cg_math_log2", (DL_FUNC) &cg_math_log2, 1},
  {"cg_math_log2_grad", (DL_FUNC) &cg_math_log2_grad, 2},
  {"cg_math_log10", (DL_FUNC) &cg_math_log10, 1},
  {"cg_math_log10_grad", (DL_FUNC) &cg_math_log10_grad, 2},
  {"cg_math_abs", (DL_FUNC) &cg_math_abs, 1},
  {"cg_math_abs_grad", (DL_FUNC) &cg_math_abs_grad, 3},
  {"cg_math_sin", (DL_FUNC) &cg_math_sin, 1},
  {"cg_math_sin_grad", (DL_FUNC) &cg_math_sin_grad, 2},
  {"cg_math_cos", (DL_FUNC) &cg_math_cos, 1},
  {"cg_math_cos_grad", (DL_FUNC) &cg_math_cos_grad, 2},
  {"cg_math_tan", (DL_FUNC) &cg_math_tan, 1},
  {"cg_math_tan_grad", (DL_FUNC) &cg_math_tan_grad, 2},
  {"cg_math_sinh", (DL_FUNC) &cg_math_sinh, 1},
  {"cg_math_sinh_grad", (DL_FUNC) &cg_math_sinh_grad, 2},
  {"cg_math_cosh", (DL_FUNC) &cg_math_cosh, 1},
  {"cg_math_cosh_grad", (DL_FUNC) &cg_math_cosh_grad, 2},
  {"cg_math_tanh", (DL_FUNC) &cg_math_tanh, 1},
  {"cg_math_tanh_grad", (DL_FUNC) &cg_math_tanh_grad, 3},
  {"cg_math_asin", (DL_FUNC) &cg_math_asin, 1},
  {"cg_math_asin_grad", (DL_FUNC) &cg_math_asin_grad, 2},
  {"cg_math_acos", (DL_FUNC) &cg_math_acos, 1},
  {"cg_math_acos_grad", (DL_FUNC) &cg_math_acos_grad, 2},
  {"cg_math_atan", (DL_FUNC) &cg_math_atan, 1},
  {"cg_math_atan_grad", (DL_FUNC) &cg_math_atan_grad, 2},
  {"cg_math_asinh", (DL_FUNC) &cg_math_asinh, 1},
  {"cg_math_asinh_grad", (DL_FUNC) &cg_math_asinh_grad, 2},
  {"cg_math_acosh", (DL_FUNC) &cg_math_acosh, 1},
  {"cg_math_acosh_grad", (DL_FUNC) &cg_math_acosh_grad, 2},
  {"cg_math_atanh", (DL_FUNC) &cg_math_atanh, 1},
  {"cg_math_atanh_grad", (DL_FUNC) &cg_math_atanh_grad, 2},
  {"cg_math_sigmoid", (DL_FUNC) &cg_math_sigmoid, 1},
  {"cg_math_sigmoid_grad", (DL_FUNC) &cg_math_sigmoid_grad, 3},
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
