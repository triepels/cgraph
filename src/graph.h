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

#ifndef GRAPH_H
#define GRAPH_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"
#include "symbols.h"

/*
 * INLINED GET/SET FUNCTIONS
 */

inline SEXP cg_graph_nodes(SEXP graph)
{
    SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

    if(TYPEOF(nodes) != VECSXP)
    {
        Rf_errorcall(R_NilValue, "graph does not have any nodes");
    }

    UNPROTECT(1);

    return nodes;
}

inline int cg_graph_eager(SEXP graph)
{
    SEXP eager = PROTECT(CG_GET(graph, CG_EAGER_SYMBOL));

    if(!IS_SCALAR(eager, LGLSXP))
    {
        UNPROTECT(1);

        return 1;
    }

    UNPROTECT(1);

    return INTEGER(eager)[0];
}

inline void cg_graph_set_eager(SEXP graph, const int eager)
{
    CG_SET(graph, CG_EAGER_SYMBOL, Rf_ScalarLogical(eager));
}

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_graph_get(SEXP graph, SEXP name);

void cg_graph_add_node(SEXP graph, SEXP node);

SEXP cg_graph_forward(SEXP graph, SEXP target);

SEXP cg_graph_backward(SEXP graph, SEXP target, SEXP index);

SEXP cg_graph_print(SEXP graph);

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_graph();

#endif
