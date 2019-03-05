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

#ifndef NODE_H
#define NODE_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"

/*
 * PRIVATE METHODS
 */

const char* cg_node_name(SEXP node);

void cg_node_set_name(SEXP node, const char *name);

SEXP cg_node_symbol(SEXP node);

int cg_node_id(SEXP node);

void cg_node_set_id(SEXP node, const int id);

// Note: this function allocates when unique = TRUE
SEXP cg_node_inputs(SEXP node, int unique);

void cg_node_add_input(SEXP node, SEXP input);

// Note: this function allocates when unique = TRUE
SEXP cg_node_outputs(SEXP node, int unique);

void cg_node_add_output(SEXP node, SEXP output);

SEXP cg_node_value(SEXP node);

void cg_node_set_value(SEXP node, SEXP value);

SEXP cg_node_function(SEXP node);

void cg_node_set_function(SEXP node, SEXP function);

void cg_node_eval(SEXP node, SEXP values);

void cg_node_eval_gradient(SEXP node, SEXP values, SEXP gradients);

/*
 * PRIVATE CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, const char *name);

SEXP cg_parameter(SEXP value, const char *name);

SEXP cg_input(const char *name);

SEXP cg_operator(SEXP function, SEXP inputs, const char *name);

#endif
