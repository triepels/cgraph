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

#ifndef NODE_H
#define NODE_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"

/*
 * TYPES
 */

#define CGCST 0
#define CGPRM 1
#define CGIPT 2
#define CGOPR 3

/*
 * PRIVATE METHODS
 */

const char* cg_node_name(SEXP node);

void cg_node_set_name(SEXP node, const char *name);

int cg_node_id(SEXP node);

void cg_node_set_id(SEXP node, const int id);

int cg_node_type(SEXP node);

void cg_node_set_type(SEXP node, const int type);

SEXP cg_node_inputs(SEXP node);

void cg_node_set_inputs(SEXP node, SEXP inputs);

SEXP cg_node_value(SEXP node);

void cg_node_set_value(SEXP node, SEXP value);

SEXP cg_node_grad(SEXP node);

void cg_node_set_grad(SEXP node, SEXP value);

SEXP cg_node_function(SEXP node);

void cg_node_set_function(SEXP node, SEXP function);

/*
 * PRIVATE CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, SEXP name);

SEXP cg_parameter(SEXP value, SEXP name);

SEXP cg_input(SEXP name);

SEXP cg_operator(SEXP function, SEXP inputs, SEXP name);

#endif
