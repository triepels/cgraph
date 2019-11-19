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

#ifndef GRAPH_H
#define GRAPH_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * PRIVATE METHODS
 */

SEXP cg_graph_nodes(SEXP graph);

char* cg_graph_gen_name(SEXP graph); /* NOTE: DEPRECATED */

int cg_graph_node_exists(SEXP graph, const char *name);

void cg_graph_add_node(SEXP graph, SEXP node);

SEXP cg_graph_get_node(SEXP graph, const int id);

SEXP cg_graph_reverse_dfs(SEXP graph, SEXP target); /* NOTE: DEPRECATED */

/*
 * PUBLIC METHODS
 */

SEXP cg_graph_forward(SEXP graph, SEXP target);

SEXP cg_graph_backward(SEXP graph, SEXP target);

SEXP cg_graph_run(SEXP graph, SEXP target, SEXP values); /* NOTE: DEPRECATED */

SEXP cg_graph_gradients(SEXP graph, SEXP target, SEXP values, SEXP gradients, SEXP index); /* NOTE: DEPRECATED */

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_graph();

SEXP cg_graph_print(SEXP graph);

#endif
