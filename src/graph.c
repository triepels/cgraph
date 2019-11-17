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

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "node.h"
#include "class.h"
#include "graph.h"
#include "stack.h"
#include "session.h"

/*
 * SYMBOLS
 */

#define CG_NODES_SYMBOL Rf_install("nodes")

/*
 * PRIVATE METHODS
 */

SEXP cg_graph_nodes(SEXP graph)
{
  SEXP nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL);

  if(TYPEOF(nodes) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "graph does not have any nodes");
  }

  return nodes;
}

char* cg_graph_gen_name(SEXP graph)
{
  char *name = R_alloc(1, 32 * sizeof(char));

  SEXP nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL);

  if(TYPEOF(nodes) != VECSXP)
  {
    strcpy(name, "v1");
  }
  else
  {
    R_len_t n = XLENGTH(nodes);

    do
    {
      sprintf(name, "v%d", ++n);

    } while (cg_graph_node_exists(graph, name));
  }

  return name;
}

int cg_graph_node_exists(SEXP graph, const char *name)
{
  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, CG_NODES_SYMBOL));

  if(TYPEOF(nodes) == VECSXP)
  {
    SEXP names = Rf_getAttrib(nodes, R_NamesSymbol);

    if(!Rf_isString(names))
    {
      Rf_errorcall(R_NilValue, "graph has invalid node names");
    }

    R_len_t n = XLENGTH(names);

    for(int i = 0; i < n; i++)
    {
      SEXP node_name = STRING_ELT(names, i);

      if(strcmp(CHAR(node_name), name) == 0)
      {
        UNPROTECT(1);

        return TRUE;
      }
    }
  }

  UNPROTECT(1);

  return FALSE;
}

void cg_graph_add_node(SEXP graph, SEXP node)
{
  const char *name = cg_node_name(node);

  if(cg_graph_node_exists(graph, name))
  {
    Rf_errorcall(R_NilValue, "'%s' is already defined in the graph", name);
  }

  int index_nodes;

  SEXP nodes = R_NilValue;

  PROTECT_WITH_INDEX(nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL), &index_nodes);

  if(TYPEOF(nodes) != VECSXP)
  {
    REPROTECT(nodes = Rf_allocVector(VECSXP, 1), index_nodes);

    SET_VECTOR_ELT(nodes, 0, node);

    Rf_setAttrib(nodes, R_NamesSymbol, Rf_mkString(name));

    cg_node_set_id(node, 1);
  }
  else
  {
    int index_names;

    SEXP names = R_NilValue;

    PROTECT_WITH_INDEX(names = Rf_getAttrib(nodes, R_NamesSymbol), &index_names);

    if(!Rf_isString(names))
    {
      Rf_errorcall(R_NilValue, "graph has invalid node names");
    }

    R_len_t n = XLENGTH(nodes);

    REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), index_nodes);

    REPROTECT(names = Rf_lengthgets(names, n + 1), index_names);

    SET_VECTOR_ELT(nodes, n, node);

    SET_STRING_ELT(names, n, Rf_mkChar(name));

    Rf_setAttrib(nodes, R_NamesSymbol, names);

    cg_node_set_id(node, n + 1);

    UNPROTECT(1);
  }

  Rf_defineVar(CG_NODES_SYMBOL, nodes, graph);

  UNPROTECT(1);
}

SEXP cg_graph_get_node(SEXP graph, const int id)
{
  SEXP nodes = cg_graph_nodes(graph);

  if(id < 1 || id > XLENGTH(nodes))
  {
    Rf_errorcall(R_NilValue, "cannot find node with id %d", id);
  }

  SEXP node = VECTOR_ELT(nodes, id - 1);

  if(!cg_is(node, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "invalid node at index %d", id);
  }

  return node;
}

SEXP cg_graph_reverse_dfs(SEXP graph, SEXP target)
{
  SEXP nodes = cg_graph_nodes(graph);

  R_len_t n = XLENGTH(nodes);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

  int *visited = (int*)R_alloc(n, sizeof(int));

  for(int i = 0; i < n; i++)
  {
    visited[i] = 0;
  }

  cg_stack_t *stack = cg_stack_allocate(n);

  int id = cg_node_id(target);

  cg_stack_push(stack, id);

  visited[id - 1] = 1;

  int k = 0;

  while(!cg_stack_is_empty(stack))
  {
    int done = 1;

    id = cg_stack_top(stack);

    SEXP node = cg_graph_get_node(graph, id);

    SEXP inputs = PROTECT(cg_node_inputs(node));

    if(!Rf_isNull(inputs))
    {
      R_len_t m = XLENGTH(inputs);

      for(int i = 0; i < m; i++)
      {
        SEXP input = VECTOR_ELT(inputs, i);

        int input_id = cg_node_id(input);

        if(input_id < 1 || input_id > n)
        {
          Rf_errorcall(R_NilValue, "unable to retrieve node with id %d", input_id);
        }

        if(!visited[input_id - 1])
        {
          cg_stack_push(stack, input_id);

          visited[input_id - 1] = 1;

          done = 0;

          break;
        }
      }
    }

    if(done)
    {
      cg_stack_pop(stack);

      SET_VECTOR_ELT(out, k, node);

      k++;
    }

    UNPROTECT(1);
  }

  SETLENGTH(out, k);

  UNPROTECT(1);

  return out;
}

/*
 * PUBLIC METHODS
 */

SEXP cg_graph_run(SEXP graph, SEXP target, SEXP values)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(target, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a cg_node object");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "argument 'values' must be an environment");
  }

  SEXP nodes = PROTECT(cg_graph_reverse_dfs(graph, target));

  R_len_t n = XLENGTH(nodes);

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(cg_node_type(node) != CGIPT)
    {
      cg_node_eval(node, values);
    }
  }

  UNPROTECT(1);

  return values;
}

SEXP cg_graph_gradients(SEXP graph, SEXP target, SEXP values, SEXP gradients, SEXP index)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(target, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a cg_node object");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "argument 'values' must be an environment");
  }

  if(!Rf_isEnvironment(gradients))
  {
    Rf_errorcall(R_NilValue, "argument 'gradients' must be an environment");
  }

  if(!Rf_isNull(index) && (!Rf_isNumeric(index) || XLENGTH(index) < 1))
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be NULL or a numeric scalar");
  }

  SEXP nodes = PROTECT(cg_graph_reverse_dfs(graph, target));

  R_len_t n = XLENGTH(nodes);

  SEXP root = VECTOR_ELT(nodes, n - 1);

  SEXP symbol = cg_node_symbol(root);

  SEXP value = PROTECT(Rf_findVarInFrame(values, symbol));

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "cannot differentiate type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(value)), cg_node_name(target));
  }

  R_len_t m = XLENGTH(value);

  SEXP gradient = PROTECT(Rf_allocVector(REALSXP, m));

  double *y = REAL(gradient);

  memset(y, 0, m * sizeof(double));

  if(Rf_isNull(index))
  {
    for(int i = 0; i < m; i++)
    {
      y[i] = 1;
    }
  }
  else
  {
    int k = Rf_asInteger(index);

    if(k < 1 || k > m)
    {
      Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at index %d",
                   cg_node_name(root), k);
    }

    y[k - 1] = 1;
  }

  SHALLOW_DUPLICATE_ATTRIB(gradient, value);

  Rf_defineVar(symbol, gradient, gradients);

  for(int i = n - 1; i >= 0; i--)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(cg_node_type(node) == CGOPR)
    {
      cg_node_eval_gradients(node, values, gradients);
    }
  }

  UNPROTECT(3);

  return gradients;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_graph()
{
  SEXP graph = PROTECT(cg_class1("cg_graph"));

  cg_session_set_graph(graph);

  UNPROTECT(1);

  return graph;
}
