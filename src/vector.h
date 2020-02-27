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

#ifndef VECTOR_H
#define VECTOR_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_pos_def(SEXP x, SEXP out);
SEXP cg_pos_grad(SEXP x, SEXP grad, SEXP out);
SEXP cg_neg_def(SEXP x, SEXP out);
SEXP cg_neg_grad(SEXP x, SEXP grad, SEXP out);
SEXP cg_add_def(SEXP x, SEXP y, SEXP out);
SEXP cg_sub_def(SEXP x, SEXP y, SEXP out);
SEXP cg_mul_def(SEXP x, SEXP y, SEXP out);
SEXP cg_mul_grad_x(SEXP x, SEXP y, SEXP grad, SEXP out);
SEXP cg_mul_grad_y(SEXP x, SEXP y, SEXP grad, SEXP out);
SEXP cg_div_def(SEXP x, SEXP y, SEXP out);
SEXP cg_div_grad_x(SEXP x, SEXP y, SEXP grad, SEXP out);
SEXP cg_div_grad_y(SEXP x, SEXP y, SEXP grad, SEXP out);
SEXP cg_pow_def(SEXP x, SEXP y, SEXP out);
SEXP cg_pow_grad_x(SEXP x, SEXP y, SEXP grad, SEXP out);
SEXP cg_pow_grad_y(SEXP x, SEXP y, SEXP grad, SEXP out);
SEXP cg_square_def(SEXP x, SEXP out);
SEXP cg_square_grad(SEXP x, SEXP grad, SEXP out);
SEXP cg_sqrt_def(SEXP x, SEXP out);
SEXP cg_sqrt_grad(SEXP x, SEXP value, SEXP grad, SEXP out);
SEXP cg_cbrt_def(SEXP x, SEXP out);
SEXP cg_cbrt_grad(SEXP x, SEXP value, SEXP grad, SEXP out);
SEXP cg_exp_def(SEXP x, SEXP out);
SEXP cg_exp_grad(SEXP x, SEXP value, SEXP grad, SEXP out);
SEXP cg_exp2_def(SEXP x, SEXP out);
SEXP cg_exp2_grad(SEXP x, SEXP value, SEXP grad, SEXP out);
SEXP cg_ln_def(SEXP x, SEXP out);
SEXP cg_ln_grad(SEXP x, SEXP grad, SEXP out);
SEXP cg_log2_def(SEXP x, SEXP out);
SEXP cg_log2_grad(SEXP x, SEXP grad, SEXP out);
SEXP cg_log10_def(SEXP x, SEXP out);
SEXP cg_log10_grad(SEXP x, SEXP grad, SEXP out);
SEXP cg_abs_def(SEXP x, SEXP out);
SEXP cg_abs_grad(SEXP x, SEXP value, SEXP grad, SEXP out);
SEXP cg_sin_def(SEXP x, SEXP out);
SEXP cg_sin_grad(SEXP x, SEXP grad, SEXP out);

SEXP cg_sigmoid_def(SEXP x, SEXP out);
SEXP cg_sigmoid_grad(SEXP value, SEXP grad, SEXP out);

#endif
