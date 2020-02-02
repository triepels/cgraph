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

#ifndef SYMBOLS_H
#define SYMBOLS_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

SEXP CG_ID_SYMBOL;
SEXP CG_DEF_SYMBOL;
SEXP CG_FUN_SYMBOL;
SEXP CG_GRAD_SYMBOL;
SEXP CG_NAME_SYMBOL;
SEXP CG_TYPE_SYMBOL;
SEXP CG_EAGER_SYMBOL;
SEXP CG_GRADS_SYMBOL;
SEXP CG_GRAPH_SYMBOL;
SEXP CG_NODES_SYMBOL;
SEXP CG_VALUE_SYMBOL;
SEXP CG_INPUTS_SYMBOL;
SEXP CG_SESSION_SYMBOL;

#endif
