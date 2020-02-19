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

SEXP cg_add_def(SEXP x, SEXP y, SEXP out);

SEXP cg_sin_def(SEXP x, SEXP out);
SEXP cg_sin_grad(SEXP x, SEXP grad, SEXP out);

SEXP cg_sigmoid_def(SEXP x, SEXP out);
SEXP cg_sigmoid_grad(SEXP value, SEXP grad, SEXP out);

#endif
