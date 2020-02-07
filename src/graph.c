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

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "node.h"
#include "class.h"
#include "graph.h"
#include "stack.h"
#include "session.h"
#include "function.h"

/*
 * CLASS DEFINITIONS
 */

static const cg_class_def_t GRAPH_DEF[] = {
  { "eager",  1 },
  { "nodes",  1 },
  { NULL,     0 }
};

/*
 * PRIVATE METHODS
 */

void cg_graph_add_node(SEXP graph, SEXP node)
{
  int index;

  SEXP nodes = R_NilValue;

  PROTECT_WITH_INDEX(nodes = CG_GET(graph, CG_NODES_SYMBOL), &index);

  cg_class_unlock(graph, GRAPH_DEF);

  if(TYPEOF(nodes) != VECSXP)
  {
    REPROTECT(nodes = Rf_allocVector(VECSXP, 1), index);

    SET_VECTOR_ELT(nodes, 0, node);

    CG_SET_INT(node, CG_ID_SYMBOL, 1);
  }
  else
  {
    R_len_t n = XLENGTH(nodes);

    REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), index);

    SET_VECTOR_ELT(nodes, n, node);

    CG_SET_INT(node, CG_ID_SYMBOL, n + 1);
  }

  CG_SET(graph, CG_NODES_SYMBOL, nodes);

  cg_class_lock(graph, GRAPH_DEF);

  UNPROTECT(1);
}

void cg_graph_dfs_from(SEXP graph, SEXP target, int (*filter)(SEXP node), void (*exec)(SEXP node))
{
  SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

  int id = CG_GET_INT(target, CG_ID_SYMBOL);

  R_len_t n = XLENGTH(nodes);

  int *visited = (int*)R_alloc(n, sizeof(int));

  memset(visited, 0, n * sizeof(int));

  cg_stack_t *stack = cg_stack_allocate(n);

  cg_stack_push(stack, target);

  visited[id - 1] = 1;

  while(!cg_stack_is_empty(stack))
  {
    int can_traverse = 0;

    SEXP node = cg_stack_top(stack);

    SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

    R_len_t m = XLENGTH(inputs);

    for(int i = 0; i < m; i++)
    {
      SEXP input = VECTOR_ELT(inputs, i);

      int input_id = CG_GET_INT(input, CG_ID_SYMBOL);

      if(!visited[input_id - 1] && filter(input))
      {
        cg_stack_push(stack, input);

        visited[input_id - 1] = 1;

        can_traverse = 1;

        break;
      }
    }

    if(!can_traverse)
    {
      cg_stack_pop(stack);

      exec(node);
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
}

void cg_graph_reverse_dfs_from(SEXP graph, SEXP target, int (*filter)(SEXP node), void (*exec)(SEXP node))
{
  SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

  int id = CG_GET_INT(target, CG_ID_SYMBOL);

  R_len_t n = XLENGTH(nodes);

  int *visited = (int*)R_alloc(n, sizeof(int));

  memset(visited, 0, n * sizeof(int));

  SEXP *queue = (SEXP*)R_alloc(n, sizeof(SEXP));

  cg_stack_t *stack = cg_stack_allocate(n);

  cg_stack_push(stack, target);

  visited[id - 1] = 1;

  int k = 0;

  while(!cg_stack_is_empty(stack))
  {
    int can_traverse = 0;

    SEXP node = cg_stack_top(stack);

    SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

    R_len_t m = XLENGTH(inputs);

    for(int i = 0; i < m; i++)
    {
      SEXP input = VECTOR_ELT(inputs, i);

      int input_id = CG_GET_INT(input, CG_ID_SYMBOL);

      if(!visited[input_id - 1] && filter(input))
      {
        cg_stack_push(stack, input);

        visited[input_id - 1] = 1;

        can_traverse = 1;

        break;
      }
    }

    if(!can_traverse)
    {
      cg_stack_pop(stack);

      queue[k++] = node;
    }

    UNPROTECT(1);
  }

  for(int i = k - 1; i >= 0; i--)
  {
    exec(queue[i]);
  }

  UNPROTECT(1);
}

/*
 * PUBLIC METHODS
 */

SEXP cg_graph_get(SEXP graph, SEXP name)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

  const char *pn = CHAR(STRING_ELT(name, 0));

  if(TYPEOF(nodes) == VECSXP)
  {
    R_len_t n = XLENGTH(nodes);

    for(int i = n - 1; i >= 0; i--)
    {
      SEXP node = VECTOR_ELT(nodes, i);

      if(strcmp(CG_GET_STR(node, CG_NAME_SYMBOL), pn) == 0)
      {
        UNPROTECT(1);

        return node;
      }
    }
  }

  Rf_errorcall(R_NilValue, "cannot find node '%s'", pn);
}

static int filter(SEXP node)
{
  if(CG_GET_INT(node, CG_TYPE_SYMBOL) == CGOPR)
  {
    return 1;
  }

  return 0;
}

SEXP cg_graph_forward(SEXP graph, SEXP target)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(target, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a cg_node object");
  }

  if(CG_GET_INT(target, CG_TYPE_SYMBOL) != CGOPR)
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be an operator node");
  }

  cg_graph_dfs_from(graph, target, filter, cg_node_forward);

  return R_NilValue;
}

SEXP cg_graph_backward(SEXP graph, SEXP target, SEXP index)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(target, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a cg_node object");
  }

  if(CG_GET_INT(target, CG_TYPE_SYMBOL) != CGOPR)
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be an operator node");
  }

  if(!Rf_isNull(index) && (!Rf_isNumeric(index) || XLENGTH(index) < 1))
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be NULL or a numeric scalar");
  }

  SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

  R_len_t n = XLENGTH(nodes);

  for(int i = 0; i < n; i++)
  {
    CG_SET(VECTOR_ELT(nodes, i), CG_GRAD_SYMBOL, R_NilValue);
  }

  SEXP value = PROTECT(CG_GET(target, CG_VALUE_SYMBOL));

  R_len_t m = XLENGTH(value);

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(value)), CG_GET_STR(target, CG_NAME_SYMBOL));
  }

  SEXP grad = PROTECT(Rf_allocVector(REALSXP, m));

  double *pg = REAL(grad);

  memset(pg, 0, m * sizeof(double));

  if(!Rf_isNull(index))
  {
    int k = Rf_asInteger(index);

    if(k < 1 || k > m)
    {
      Rf_errorcall(R_NilValue, "argument 'index' out of bounds");
    }

    pg[k - 1] = 1;
  }
  else
  {
    for(int i = 0; i < m; i++)
    {
      pg[i] = 1;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(grad, value);

  CG_SET(target, CG_GRAD_SYMBOL, grad);

  cg_graph_reverse_dfs_from(graph, target, filter, cg_node_backward);

  UNPROTECT(3);

  return R_NilValue;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_graph(SEXP eager)
{
  if(!IS_SCALAR(eager, LGLSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eager' must be a logical scalar");
  }

  SEXP graph = PROTECT(cg_class("cg_graph", GRAPH_DEF));

  CG_SET(graph, CG_EAGER_SYMBOL, eager);

  cg_class_lock(graph, GRAPH_DEF);

  cg_session_set_graph(graph);

  UNPROTECT(1);

  return graph;
}
