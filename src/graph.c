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
    strcpy(name, "x1");
  }
  else
  {
    R_len_t n = Rf_xlength(nodes);

    do
    {
      sprintf(name, "x%d", ++n);

    } while (cg_graph_node_exists(graph, name));
  }

  return name;
}

int cg_graph_node_exists(SEXP graph, const char *name)
{
  SEXP nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL);

  if(TYPEOF(nodes) == VECSXP)
  {
    SEXP names = Rf_getAttrib(nodes, R_NamesSymbol);

    R_len_t n = Rf_xlength(names);

    for(int i = 0; i < n; i++)
    {
      SEXP node_name = STRING_ELT(names, i);

      if(strcmp(CHAR(node_name), name) == 0)
      {
        return TRUE;
      }
    }
  }

  return FALSE;
}

void cg_graph_add_node(SEXP graph, SEXP node)
{
  const char *name = cg_node_name(node);

  if(cg_graph_node_exists(graph, name))
  {
    Rf_errorcall(R_NilValue, "'%s' is already defined in the graph", name);
  }

  SEXP nodes = Rf_findVarInFrame(graph, CG_NODES_SYMBOL);

  SEXP names = Rf_getAttrib(nodes, R_NamesSymbol);

  R_len_t n = Rf_xlength(nodes);

  if(TYPEOF(nodes) != VECSXP)
  {
    nodes = PROTECT(Rf_allocVector(VECSXP, 1));

    names = PROTECT(Rf_mkString(name));

    SET_VECTOR_ELT(nodes, 0, node);

    Rf_setAttrib(nodes, R_NamesSymbol, names);

    cg_node_set_id(node, 1);
  }
  else
  {
    nodes = PROTECT(Rf_lengthgets(nodes, n + 1));

    names = PROTECT(Rf_lengthgets(names, n + 1));

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

int* cg_graph_forward_dep(SEXP graph, SEXP target, int *length)
{
  SEXP nodes = cg_graph_nodes(graph);

  R_len_t n = Rf_xlength(nodes);

  int *ids = malloc(n * sizeof(int));

  int *visited = calloc(n, sizeof(int));

  if(ids == NULL || visited == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate memory");
  }

  cg_stack *stack = cg_stack_allocate(n);

  cg_stack_push(stack, cg_node_id(target));

  (*length) = 0;

  while(!cg_stack_is_empty(stack))
  {
    int current = cg_stack_top(stack);

    SEXP node = cg_graph_get_node(graph, current);

    SEXP outputs = cg_node_outputs(node, FALSE);

    R_len_t m = Rf_xlength(outputs);

    if(visited[current - 1] == 0)
    {
      if(m > 0)
      {
        for(int i = 0; i < m; i++)
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

        ids[*length] = current;

        (*length)++;
      }
    }
    else
    {
      if(visited[current - 1] == 1 & m > 0)
      {
        ids[*length] = current;

        (*length)++;
      }

      cg_stack_pop(stack);
    }

    visited[current - 1]++;
  }

  cg_stack_destroy(stack);

  free(visited);

  return ids;
}

int* cg_graph_backward_dep(SEXP graph, SEXP target, int *length)
{
  SEXP nodes = cg_graph_nodes(graph);

  R_len_t n = Rf_xlength(nodes);

  int *ids = malloc(n * sizeof(int));

  int *visited = calloc(n, sizeof(int));

  if(ids == NULL || visited == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate memory");
  }

  cg_stack *stack = cg_stack_allocate(n);

  cg_stack_push(stack, cg_node_id(target));

  (*length) = 0;

  while(!cg_stack_is_empty(stack))
  {
    int current = cg_stack_top(stack);

    SEXP node = cg_graph_get_node(graph, current);

    SEXP inputs = cg_node_inputs(node, FALSE);

    R_len_t m = Rf_xlength(inputs);

    if(visited[current - 1] == 0)
    {
      if(m > 0)
      {
        for(int i = 0; i < m; i++)
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

        ids[*length] = current;

        (*length)++;
      }
    }
    else
    {
      if(visited[current - 1] == 1 & m > 0)
      {
        ids[*length] = current;

        (*length)++;
      }

      cg_stack_pop(stack);
    }

    visited[current - 1]++;
  }

  cg_stack_destroy(stack);

  free(visited);

  return ids;
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

  int n;

  int *ids = cg_graph_backward_dep(graph, target, &n);

  SEXP nodes = cg_graph_nodes(graph);

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, ids[i] - 1);

    if(!cg_is(node, "cg_input"))
    {
      cg_node_eval(node, values);
    }
  }

  free(ids);

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

  int n, k = Rf_asInteger(index);

  int *ids = cg_graph_backward_dep(graph, target, &n);

  SEXP nodes = cg_graph_nodes(graph);

  if(n > 0)
  {
    SEXP root = VECTOR_ELT(nodes, ids[n - 1] - 1);

    SEXP symbol = cg_node_symbol(root);

    SEXP value = Rf_findVarInFrame(values, symbol);

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
      SEXP node = VECTOR_ELT(nodes, ids[i] - 1);

      if(cg_is(node, "cg_operator") || cg_is(node, "cg_parameter"))
      {
        cg_node_eval_gradient(node, values, gradients);
      }
    }

    UNPROTECT(1);
  }

  free(ids);

  return gradients;
}

SEXP cg_graph_add_constant(SEXP graph, SEXP value, SEXP name)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  SEXP constant = R_NilValue;

  if(Rf_isNull(name))
  {
    char *gen_name = cg_graph_gen_name(graph);

    constant = PROTECT(cg_constant(value, gen_name));

    free(gen_name);
  }
  else
  {
    constant = PROTECT(cg_constant(value, CHAR(STRING_ELT(name, 0))));
  }

  cg_graph_add_node(graph, constant);

  UNPROTECT(1);

  return constant;
}

SEXP cg_graph_add_parameter(SEXP graph, SEXP value, SEXP name)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  SEXP parameter = R_NilValue;

  if(Rf_isNull(name))
  {
    char *gen_name = cg_graph_gen_name(graph);

    parameter = PROTECT(cg_parameter(value, gen_name));

    free(gen_name);
  }
  else
  {
    parameter = PROTECT(cg_parameter(value, CHAR(STRING_ELT(name, 0))));
  }

  cg_graph_add_node(graph, parameter);

  UNPROTECT(1);

  return parameter;
}

SEXP cg_graph_add_input(SEXP graph, SEXP name)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  SEXP input = R_NilValue;

  if(Rf_isNull(name))
  {
    char *gen_name = cg_graph_gen_name(graph);

    input = PROTECT(cg_input(gen_name));

    free(gen_name);
  }
  else
  {
    input = PROTECT(cg_input(CHAR(STRING_ELT(name, 0))));
  }

  cg_graph_add_node(graph, input);

  UNPROTECT(1);

  return input;
}

SEXP cg_graph_add_operator(SEXP graph, SEXP function, SEXP inputs, SEXP name)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(function, "cg_function"))
  {
    Rf_errorcall(R_NilValue, "argument 'function' must be a cg_function object");
  }

  if(TYPEOF(inputs) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'inputs' must be a list");
  }

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  R_xlen_t n = Rf_xlength(inputs);

  int m = 0;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(!cg_is(input, "cg_node"))
    {
      input = PROTECT(cg_graph_add_constant(graph, input, R_NilValue));

      SET_VECTOR_ELT(inputs, i, input);

      m++;
    }
  }

  SEXP op = R_NilValue;

  if(Rf_isNull(name))
  {
    char *gen_name = cg_graph_gen_name(graph);

    op = PROTECT(cg_operator(function, inputs, gen_name));

    free(gen_name);
  }
  else
  {
    op = PROTECT(cg_operator(function, inputs, CHAR(STRING_ELT(name, 0))));
  }

  cg_graph_add_node(graph, op);

  UNPROTECT(1 + m);

  return op;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_graph()
{
  return cg_class1("cg_graph");
}
