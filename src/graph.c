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
#include "symbols.h"
#include "function.h"

/*
 * PRIVATE METHODS
 */

SEXP cg_graph_nodes(SEXP graph)
{
  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, CG_NODES_SYMBOL));

  if(TYPEOF(nodes) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "graph does not have any nodes");
  }

  UNPROTECT(1);

  return nodes;
}

int cg_graph_eager(SEXP graph)
{
  SEXP eager = PROTECT(Rf_findVarInFrame(graph, CG_EAGER_SYMBOL));

  if(!IS_SCALAR(eager, LGLSXP))
  {
    UNPROTECT(1);

    return 1;
  }

  UNPROTECT(1);

  return INTEGER(eager)[0];
}

void cg_graph_set_eager(SEXP graph, const int eager)
{
  Rf_defineVar(CG_EAGER_SYMBOL, Rf_ScalarLogical(eager), graph);
}

char* cg_graph_gen_name(SEXP graph)
{
  char *name = R_alloc(1, 32 * sizeof(char));

  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, CG_NODES_SYMBOL));

  if(TYPEOF(nodes) != VECSXP)
  {
    strcpy(name, "v1");
  }
  else
  {
    R_len_t n = XLENGTH(nodes);

    sprintf(name, "v%d", n + 1);
  }

  UNPROTECT(1);

  return name;
}

void cg_graph_add_node(SEXP graph, SEXP node)
{
  int index_nodes;

  SEXP nodes = R_NilValue;

  PROTECT_WITH_INDEX(nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL), &index_nodes);

  if(TYPEOF(nodes) != VECSXP)
  {
    REPROTECT(nodes = Rf_allocVector(VECSXP, 1), index_nodes);

    SET_VECTOR_ELT(nodes, 0, node);

    cg_node_set_id(node, 1);
  }
  else
  {
    R_len_t n = XLENGTH(nodes);

    REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), index_nodes);

    SET_VECTOR_ELT(nodes, n, node);

    cg_node_set_id(node, n + 1);
  }

  Rf_defineVar(CG_NODES_SYMBOL, nodes, graph);

  UNPROTECT(1);
}

void cg_graph_clear_grads(SEXP graph)
{
  SEXP nodes = PROTECT(cg_graph_nodes(graph));

  R_len_t n = XLENGTH(nodes);

  for(int i = 0; i < n; i++)
  {
    cg_node_set_grad(VECTOR_ELT(nodes, i), R_NilValue);
  }

  UNPROTECT(1);
}

void cg_graph_init_target_grad(SEXP graph, SEXP target, SEXP index)
{
  SEXP value = PROTECT(cg_node_value(target));

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "unable to differentiate object of type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(value)), cg_node_name(target));
  }

  R_len_t n = XLENGTH(value);

  SEXP grad = PROTECT(Rf_allocVector(REALSXP, n));

  double *x = REAL(grad);

  memset(x, 0, n * sizeof(double));

  if(!Rf_isNull(index))
  {
    int k = Rf_asInteger(index);

    if(k < 1 || k > n)
    {
      Rf_errorcall(R_NilValue, "argument 'index' out of bounds");
    }

    x[k - 1] = 1;
  }
  else
  {
    for(int i = 0; i < n; i++)
    {
      x[i] = 1;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(grad, value);

  cg_node_set_grad(target, grad);

  UNPROTECT(2);
}

void cg_graph_reverse_dfs_from(SEXP graph, SEXP target, int reverse, int (*filter)(SEXP node), void (*exec)(SEXP node))
{
  SEXP nodes = PROTECT(cg_graph_nodes(graph));

  int id = cg_node_id(target);

  R_len_t n = XLENGTH(nodes);

  if(id < 1 || id > n)
  {
    Rf_errorcall(R_NilValue, "unable to retrieve node with id %d", id);
  }

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

    SEXP inputs = PROTECT(cg_node_inputs(node));

    R_len_t m = XLENGTH(inputs);

    for(int i = 0; i < m; i++)
    {
      SEXP input = VECTOR_ELT(inputs, i);

      int input_id = cg_node_id(input);

      if(input_id < 1 || input_id > n)
      {
        Rf_errorcall(R_NilValue, "unable to retrieve node with id %d", input_id);
      }

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

  if(reverse)
  {
    for(int i = k - 1; i >= 0; i--)
    {
      exec(queue[i]);
    }
  }
  else
  {
    for(int i = 0; i < k; i++)
    {
      exec(queue[i]);
    }
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

  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, CG_NODES_SYMBOL));

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

static int filter(SEXP node)
{
  if(cg_node_type(node) == CGOPR)
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

  if(cg_node_type(target) != CGOPR)
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be an operator node");
  }

  cg_graph_reverse_dfs_from(graph, target, 0, filter, cg_node_forward);

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

  if(cg_node_type(target) != CGOPR)
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be an operator node");
  }

  if(!Rf_isNull(index) && (!Rf_isNumeric(index) || XLENGTH(index) < 1))
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be NULL or a numeric scalar");
  }

  cg_graph_clear_grads(graph);

  cg_graph_init_target_grad(graph, target, index);

  cg_graph_reverse_dfs_from(graph, target, 1, filter, cg_node_backward);

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

  SEXP graph = PROTECT(cg_class1("cg_graph"));

  cg_graph_set_eager(graph, INTEGER(eager)[0]);

  cg_session_set_graph(graph);

  UNPROTECT(1);

  return graph;
}
