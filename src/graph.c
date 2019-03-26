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
  char *name = malloc(32 * sizeof(char));

  if(name == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate name");
  }

  SEXP nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL);

  if(TYPEOF(nodes) != VECSXP)
  {
    strcpy(name, "v1");
  }
  else
  {
    R_len_t n = Rf_xlength(nodes);

    do
    {
      sprintf(name, "v%d", ++n);

    } while (cg_graph_node_exists(graph, name));
  }

  return name;
}

int cg_graph_node_exists(SEXP graph, const char *name)
{
  int exists = FALSE;

  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, CG_NODES_SYMBOL));

  if(TYPEOF(nodes) == VECSXP)
  {
    SEXP names = Rf_getAttrib(nodes, R_NamesSymbol);

    R_len_t n = Rf_xlength(names);

    for(int i = 0; i < n; i++)
    {
      SEXP node_name = STRING_ELT(names, i);

      if(strcmp(CHAR(node_name), name) == 0)
      {
        exists = TRUE;

        break;
      }
    }
  }

  UNPROTECT(1);

  return exists;
}

void cg_graph_add_node(SEXP graph, SEXP node)
{
  const char *name = cg_node_name(node);

  if(cg_graph_node_exists(graph, name))
  {
    Rf_errorcall(R_NilValue, "'%s' is already defined in the graph", name);
  }

  int index_nodes, index_names;

  SEXP nodes = R_NilValue, names = R_NilValue;

  PROTECT_WITH_INDEX(nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL), &index_nodes);

  PROTECT_WITH_INDEX(names = Rf_getAttrib(nodes, R_NamesSymbol), &index_names);

  R_len_t n = Rf_xlength(nodes);

  if(TYPEOF(nodes) != VECSXP)
  {
    REPROTECT(nodes = Rf_allocVector(VECSXP, 1), index_nodes);

    REPROTECT(names = Rf_mkString(name), index_names);

    SET_VECTOR_ELT(nodes, 0, node);

    Rf_setAttrib(nodes, R_NamesSymbol, names);

    cg_node_set_id(node, 1);
  }
  else
  {
    REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), index_nodes);

    REPROTECT(names = Rf_lengthgets(names, n + 1), index_names);

    SET_VECTOR_ELT(nodes, n, node);

    SET_STRING_ELT(names, n, Rf_mkChar(name));

    Rf_setAttrib(nodes, R_NamesSymbol, names);

    cg_node_set_id(node, n + 1);
  }

  Rf_defineVar(CG_NODES_SYMBOL, nodes, graph);

  UNPROTECT(2);
}

SEXP cg_graph_get_node(SEXP graph, const int id)
{
  SEXP nodes = cg_graph_nodes(graph);

  if(id < 1 || id > Rf_xlength(nodes))
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

SEXP cg_graph_forward_dep(SEXP graph, SEXP target)
{
  SEXP nodes = cg_graph_nodes(graph);

  R_len_t n = Rf_xlength(nodes);

  SEXP dep = PROTECT(Rf_allocVector(VECSXP, n));

  int *visited = calloc(n, sizeof(int));

  if(visited == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate array of %d elements", n);
  }

  cg_stack *stack = cg_stack_allocate(n);

  cg_stack_push(stack, cg_node_id(target));

  int m = 0;

  while(!cg_stack_is_empty(stack))
  {
    int current = cg_stack_top(stack);

    SEXP node = cg_graph_get_node(graph, current);

    SEXP outputs = PROTECT(cg_node_outputs(node, FALSE));

    R_len_t p = Rf_xlength(outputs);

    if(visited[current - 1] == 0)
    {
      if(p > 0)
      {
        for(int i = 0; i < p; i++)
        {
          SEXP output = VECTOR_ELT(outputs, i);

          int id = cg_node_id(output);

          if(id < 1 || id > n)
          {
            Rf_warningcall(R_NilValue, "unable to retrieve node with id %d", id);
          }
          else
          {
            if(visited[id - 1] == 0)
            {
              cg_stack_push(stack, id);
            }
          }
        }
      }
      else
      {
        cg_stack_pop(stack);

        SET_VECTOR_ELT(dep, m, node);

        m++;
      }
    }
    else
    {
      if(visited[current - 1] == 1 && p > 0)
      {
        SET_VECTOR_ELT(dep, m, node);

        m++;
      }

      cg_stack_pop(stack);
    }

    visited[current - 1]++;

    UNPROTECT(1);
  }

  cg_stack_destroy(stack);

  SETLENGTH(dep, m);

  free(visited);

  UNPROTECT(1);

  return dep;
}

SEXP cg_graph_backward_dep(SEXP graph, SEXP target)
{
  SEXP nodes = cg_graph_nodes(graph);

  R_len_t n = Rf_xlength(nodes);

  SEXP dep = PROTECT(Rf_allocVector(VECSXP, n));

  int *visited = calloc(n, sizeof(int));

  if(visited == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate array of %d elements", n);
  }

  cg_stack *stack = cg_stack_allocate(n);

  cg_stack_push(stack, cg_node_id(target));

  int m = 0;

  while(!cg_stack_is_empty(stack))
  {
    int current = cg_stack_top(stack);

    SEXP node = cg_graph_get_node(graph, current);

    SEXP inputs = PROTECT(cg_node_inputs(node, FALSE));

    R_len_t p = Rf_xlength(inputs);

    if(visited[current - 1] == 0)
    {
      if(p > 0)
      {
        for(int i = 0; i < p; i++)
        {
          SEXP input = VECTOR_ELT(inputs, i);

          int id = cg_node_id(input);

          if(id < 1 || id > n)
          {
            Rf_warningcall(R_NilValue, "unable to retrieve node with id %d", id);
          }
          else
          {
            if(visited[id - 1] == 0)
            {
              cg_stack_push(stack, id);
            }
          }
        }
      }
      else
      {
        cg_stack_pop(stack);

        SET_VECTOR_ELT(dep, m, node);

        m++;
      }
    }
    else
    {
      if(visited[current - 1] == 1 && p > 0)
      {
        SET_VECTOR_ELT(dep, m, node);

        m++;
      }

      cg_stack_pop(stack);
    }

    visited[current - 1]++;

    UNPROTECT(1);
  }

  cg_stack_destroy(stack);

  SETLENGTH(dep, m);

  free(visited);

  UNPROTECT(1);

  return dep;
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

  SEXP dep = PROTECT(cg_graph_backward_dep(graph, target));

  R_len_t n = Rf_xlength(dep);

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(dep, i);

    if(!cg_is(node, "cg_input"))
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

  if(!Rf_isNumeric(index) || Rf_xlength(index) < 1)
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be a numeric scalar");
  }

  int k = Rf_asInteger(index);

  SEXP dep = PROTECT(cg_graph_backward_dep(graph, target));

  R_len_t n = Rf_xlength(dep);

  if(n > 0)
  {
    SEXP root = VECTOR_ELT(dep, n - 1);

    SEXP symbol = cg_node_symbol(root);

    SEXP value = PROTECT(Rf_findVarInFrame(values, symbol));

    if(!(Rf_isLogical(value) || Rf_isNumeric(value)))
    {
      Rf_errorcall(R_NilValue, "cannot differentiate type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(target));
    }

    R_len_t m = Rf_xlength(value);

    if(k < 1 || k > m)
    {
      Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at index %d", cg_node_name(root), k);
    }

    SEXP gradient = PROTECT(Rf_allocVector(REALSXP, m));

    memset(REAL(gradient), 0, m * sizeof(double));

    REAL(gradient)[k - 1] = 1;

    SHALLOW_DUPLICATE_ATTRIB(gradient, value);

    Rf_defineVar(symbol, gradient, gradients);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = VECTOR_ELT(dep, i);

      if(cg_is(node, "cg_operator") || cg_is(node, "cg_parameter"))
      {
        cg_node_eval_gradient(node, values, gradients);
      }
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);

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
