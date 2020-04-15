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
#include "graph.h"
#include "stack.h"
#include "session.h"
#include "function.h"

/*
 * INLINED GET/SET FUNCTIONS
 */

extern inline SEXP cg_graph_nodes(SEXP graph);

extern inline int cg_graph_eager(SEXP graph);

extern inline void cg_graph_set_eager(SEXP graph, const int eager);

/*
 * PRIVATE FUNCTIONS
 */

SEXP* cg_graph_dfs_from(SEXP graph, SEXP target, int (*filter)(SEXP node), int *k)
{
  SEXP nodes = PROTECT(cg_graph_nodes(graph));

  int id = cg_node_id(target);

  R_len_t n = XLENGTH(nodes);

  if(id < 1 || id > n)
  {
    Rf_errorcall(R_NilValue, "cannot retrieve node with id %d", id);
  }

  int *visited = Calloc(n, int);

  SEXP *queue = (SEXP*)R_alloc(n, sizeof(SEXP));

  cg_stack_t *stack = cg_stack_allocate(n);

  cg_stack_push(stack, target);

  visited[id - 1] = 1;

  *k = 0;

  while(!cg_stack_is_empty(stack))
  {
    int can_traverse = 0;

    SEXP node = cg_stack_peek(stack);

    SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

    if(TYPEOF(inputs) == VECSXP)
    {
      R_len_t m = XLENGTH(inputs);

      for(int i = 0; i < m; i++)
      {
        SEXP input = VECTOR_ELT(inputs, i);

        if(TYPEOF(input) != ENVSXP)
        {
          Rf_errorcall(R_NilValue, "node '%s' has an invalid input at index %d",
                       cg_node_name(node), i + 1);
        }

        int input_id = cg_node_id(input);

        if(input_id < 1 || input_id > n)
        {
          Rf_errorcall(R_NilValue, "cannot retrieve node with id %d", input_id);
        }

        if(!visited[input_id - 1] && filter(input))
        {
          cg_stack_push(stack, input);

          visited[input_id - 1] = 1;

          can_traverse = 1;

          break;
        }
      }
    }

    if(!can_traverse)
    {
      queue[(*k)++] = cg_stack_pop(stack);
    }

    UNPROTECT(1);
  }

  Free(visited);

  UNPROTECT(1);

  return queue;
}

static inline int forward_filter(SEXP node)
{
  cg_node_type_t type = cg_node_type(node);

  if(type == CGDOP || type == CGNOP)
  {
    return 1;
  }

  return 0;
}

static inline int backward_filter(SEXP node)
{
  cg_node_type_t type = cg_node_type(node);

  if(type == CGDOP || type == CGPRM)
  {
    return 1;
  }

  return 0;
}

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_graph_gen_name(SEXP graph)
{
  char *name = R_alloc(1, 32 * sizeof(char));

  SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

  if(TYPEOF(nodes) != VECSXP)
  {
    strcpy(name, "v1");
  }
  else
  {
    sprintf(name, "v%d", (int)XLENGTH(nodes) + 1);
  }

  UNPROTECT(1);

  return Rf_mkString(name);
}

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

      if(strcmp(cg_node_name(node), pn) == 0)
      {
        UNPROTECT(1);

        return node;
      }
    }
  }

  Rf_errorcall(R_NilValue, "cannot find node '%s'", pn);
}

void cg_graph_add_node(SEXP graph, SEXP node)
{
  int index;

  SEXP nodes = R_NilValue;

  PROTECT_WITH_INDEX(nodes = CG_GET(graph, CG_NODES_SYMBOL), &index);

  if(TYPEOF(nodes) != VECSXP)
  {
    REPROTECT(nodes = Rf_allocVector(VECSXP, 1), index);

    SET_VECTOR_ELT(nodes, 0, node);

    cg_node_set_id(node, 1);
  }
  else
  {
    R_len_t n = XLENGTH(nodes);

    REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), index);

    SET_VECTOR_ELT(nodes, n, node);

    cg_node_set_id(node, n + 1);
  }

  CG_SET(graph, CG_NODES_SYMBOL, nodes);

  UNPROTECT(1);
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

  cg_node_type_t type = cg_node_type(target);

  if(type != CGDOP && type != CGNOP)
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be an operator");
  }

  int k = 0;

  SEXP *queue = cg_graph_dfs_from(graph, target, forward_filter, &k);

  for(int i = 0; i < k; i++)
  {
    cg_node_forward(queue[i]);
  }

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

  if(cg_node_type(target) != CGDOP)
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a differentiable operator");
  }

  if(!Rf_isNull(index) && (!Rf_isNumeric(index) || XLENGTH(index) != 1))
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be NULL or a numeric scalar");
  }

  int k = 0;

  SEXP *queue = cg_graph_dfs_from(graph, target, backward_filter, &k);

  cg_node_init_grad(target, index);

  for(int i = k - 2; i >= 0; i--)
  {
    cg_node_zero_grad(queue[i]);
  }

  for(int i = k - 1; i >= 0; i--)
  {
    if(cg_node_type(queue[i]) == CGDOP)
    {
      cg_node_backward(queue[i]);
    }
  }

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

  SEXP graph = PROTECT(cg_class("cg_graph"));

  CG_SET(graph, CG_EAGER_SYMBOL, eager);

  CG_SET(graph, CG_NODES_SYMBOL, R_NilValue);

  cg_session_set_graph(graph);

  UNPROTECT(1);

  return graph;
}
