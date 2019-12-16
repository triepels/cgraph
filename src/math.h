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

#ifndef MATH_H
#define MATH_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * PUBLIC METHODS
 */

SEXP cg_math_pos(SEXP a1);
SEXP cg_math_pos_grad(SEXP a1, SEXP grad);
SEXP cg_math_neg(SEXP a1);
SEXP cg_math_neg_grad(SEXP a1, SEXP grad);
SEXP cg_math_add(SEXP a1, SEXP a2);
SEXP cg_math_add_grad(SEXP a1, SEXP grad);
SEXP cg_math_sub(SEXP a1, SEXP a2);
SEXP cg_math_sub_grad(SEXP a2, SEXP grad);
SEXP cg_math_mul(SEXP a1, SEXP a2);
SEXP cg_math_mul_grad1(SEXP a1, SEXP a2, SEXP grad);
SEXP cg_math_mul_grad2(SEXP a1, SEXP a2, SEXP grad);
SEXP cg_math_div(SEXP a1, SEXP a2);
SEXP cg_math_div_grad1(SEXP a1, SEXP a2, SEXP grad);
SEXP cg_math_div_grad2(SEXP a1, SEXP a2, SEXP grad);
SEXP cg_math_pow(SEXP a1, SEXP a2);
SEXP cg_math_pow_grad1(SEXP a1, SEXP a2, SEXP grad);
SEXP cg_math_pow_grad2(SEXP a1, SEXP a2, SEXP grad);
SEXP cg_math_square(SEXP a1);
SEXP cg_math_square_grad(SEXP a1, SEXP grad);
SEXP cg_math_sqrt(SEXP a1);
SEXP cg_math_sqrt_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_cbrt(SEXP a1);
SEXP cg_math_cbrt_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_hypot(SEXP a1, SEXP a2);
SEXP cg_math_hypot_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_fma(SEXP a1, SEXP a2, SEXP a3);
SEXP cg_math_exp(SEXP a1);
SEXP cg_math_exp_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_exp2(SEXP a1);
SEXP cg_math_exp2_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_ln(SEXP a1);
SEXP cg_math_ln_grad(SEXP a1, SEXP grad);
SEXP cg_math_log2(SEXP a1);
SEXP cg_math_log2_grad(SEXP a1, SEXP grad);
SEXP cg_math_log10(SEXP a1);
SEXP cg_math_log10_grad(SEXP a1, SEXP grad);
SEXP cg_math_abs(SEXP a1);
SEXP cg_math_abs_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_sin(SEXP a1);
SEXP cg_math_sin_grad(SEXP a1, SEXP grad);
SEXP cg_math_cos(SEXP a1);
SEXP cg_math_cos_grad(SEXP a1, SEXP grad);
SEXP cg_math_tan(SEXP a1);
SEXP cg_math_tan_grad(SEXP a1, SEXP grad);
SEXP cg_math_sinh(SEXP a1);
SEXP cg_math_sinh_grad(SEXP a1, SEXP grad);
SEXP cg_math_cosh(SEXP a1);
SEXP cg_math_cosh_grad(SEXP a1, SEXP grad);
SEXP cg_math_tanh(SEXP a1);
SEXP cg_math_tanh_grad(SEXP a1, SEXP val, SEXP grad);
SEXP cg_math_asin(SEXP a1);
SEXP cg_math_asin_grad(SEXP a1, SEXP grad);
SEXP cg_math_acos(SEXP a1);
SEXP cg_math_acos_grad(SEXP a1, SEXP grad);
SEXP cg_math_atan(SEXP a1);
SEXP cg_math_atan_grad(SEXP a1, SEXP grad);
SEXP cg_math_asinh(SEXP a1);
SEXP cg_math_asinh_grad(SEXP a1, SEXP grad);
SEXP cg_math_acosh(SEXP a1);
SEXP cg_math_acosh_grad(SEXP a1, SEXP grad);
SEXP cg_math_atanh(SEXP a1);
SEXP cg_math_atanh_grad(SEXP a1, SEXP grad);
SEXP cg_math_sigmoid(SEXP a1);
SEXP cg_math_sigmoid_grad(SEXP a1, SEXP val, SEXP grad);

#endif
