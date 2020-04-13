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

#ifndef INITIALIZER_H
#define INITIALIZER_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_init_zeros(SEXP dim, SEXP name);

SEXP cg_init_ones(SEXP dim, SEXP name);

SEXP cg_init_uniform(SEXP dim, SEXP min, SEXP max, SEXP name);

SEXP cg_init_gaussian(SEXP dim, SEXP mean, SEXP sd, SEXP name);

SEXP cg_init_xavier_uniform(SEXP dim, SEXP name);

SEXP cg_init_xavier_gaussian(SEXP dim, SEXP name);

#endif
