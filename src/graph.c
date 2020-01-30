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
 * SYMBOLS
 */

#define CG_NODES_SYMBOL Rf_install("nodes")
#define CG_EAGER_SYMBOL Rf_install("eager")
#define CG_VALUE_SYMBOL Rf_install("value")
#define CG_GRAD_SYMBOL Rf_install("grad")

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
    return 1;
  }

  UNPROTECT(1);

  return INTEGER(eager)[0];
}

void cg_graph_set_eager(SEXP graph, const int eager)
{
  SEXP graph_eager = PROTECT(Rf_ScalarLogical(eager));

  Rf_defineVar(CG_EAGER_SYMBOL, graph_eager, graph);

  UNPROTECT(1);
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

    do
    {
      sprintf(name, "v%d", ++n);
    }
    while (cg_graph_node_exists(graph, name));
  }

  UNPROTECT(1);

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
  SEXP nodes = PROTECT(cg_graph_nodes(graph));

  if(id < 1 || id > XLENGTH(nodes))
  {
    Rf_errorcall(R_NilValue, "cannot find node with id %d", id);
  }

  SEXP node = VECTOR_ELT(nodes, id - 1);

  if(!cg_is(node, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "invalid node at index %d", id);
  }

  UNPROTECT(1);

  return node;
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
    Rf_errorcall(R_NilValue, "cannot differentiate type '%s' for node '%s'",
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

static int filter(SEXP node)
{
  if(cg_node_type(node) == CGOPR)
  {
    return 1;
  }

  return 0;
}

static void forward(SEXP node)
{
  SEXP inputs = PROTECT(cg_node_inputs(node));

  R_len_t n = XLENGTH(inputs);

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

  SEXP names = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SETCAR(arg, cg_node_value(input));

    if(!Rf_isNull(names))
    {
      SEXP name = STRING_ELT(names, i);

      if(CHAR(name)[0] != '\0')
      {
        SET_TAG(arg, Rf_installChar(name));
      }
    }

    arg = CDR(arg);
  }

  SEXP function = PROTECT(cg_node_function(node));

  SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

  SEXP value = PROTECT(Rf_eval(call, R_EmptyEnv));

  cg_node_set_value(node, value);

  UNPROTECT(6);
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

  cg_graph_reverse_dfs_from(graph, target, 0, filter, forward);

  return R_NilValue;
}

static void backward(SEXP node)
{
  SEXP inputs = PROTECT(cg_node_inputs(node));

  R_len_t n = XLENGTH(inputs);

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n + 2));

  SEXP names = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SETCAR(arg, cg_node_value(input));

    if(!Rf_isNull(names))
    {
      SEXP name = STRING_ELT(names, i);

      if(CHAR(name)[0] != '\0')
      {
        SET_TAG(arg, Rf_installChar(name));
      }
    }

    arg = CDR(arg);
  }

  SETCAR(arg, cg_node_value(node));

  SET_TAG(arg, CG_VALUE_SYMBOL);

  SETCADR(arg, cg_node_grad(node));

  SET_TAG(CDR(arg), CG_GRAD_SYMBOL);

  SEXP function = PROTECT(cg_node_function(node));

  SEXP function_grads = PROTECT(cg_function_grads(function));

  R_len_t k = XLENGTH(function_grads);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(cg_node_type(input) == CGCST)
    {
      continue;
    }

    if(i >= k)
    {
      Rf_errorcall(R_NilValue, "unable to differentiate node '%s' at input %d",
                   cg_node_name(node), i + 1);
    }

    SEXP call = PROTECT(Rf_lcons(VECTOR_ELT(function_grads, i), args));

    SEXP grad = PROTECT(Rf_eval(call, R_EmptyEnv));

    if(!Rf_isNumeric(grad))
    {
      Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(node));
    }

    SEXP input_grad = PROTECT(cg_node_grad(input));

    if(Rf_isNull(input_grad))
    {
      cg_node_set_grad(input, grad);
    }
    else
    {
      R_len_t m = XLENGTH(input_grad);

      if(XLENGTH(grad) != m)
      {
        Rf_errorcall(R_NilValue, "cannot accumulate gradients of length %d and %d for node '%s'",
                     XLENGTH(grad), m, cg_node_name(node));
      }

      switch(TYPEOF(input_grad))
      {
        case REALSXP :
        {
          double *y = REAL(input_grad);

          switch(TYPEOF(grad))
          {
            case REALSXP :
            {
              double *x = REAL(grad);

              for(int j = 0; j < m; j++)
              {
                y[j] += x[j];
              }

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *x = INTEGER(grad);

              for(int j = 0; j < m; j++)
              {
                y[j] += x[j];
              }

              break;
            }
          }

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *y = INTEGER(input_grad);

          switch(TYPEOF(grad))
          {
            case REALSXP :
            {
              double *x = REAL(grad);

              for(int j = 0; j < m; j++)
              {
                y[j] += x[j];
              }

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *x = INTEGER(grad);

              for(int j = 0; j < m; j++)
              {
                y[j] += x[j];
              }

              break;
            }
          }

          break;
        }
      }
    }

    UNPROTECT(3);
  }

  UNPROTECT(5);
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

  cg_graph_reverse_dfs_from(graph, target, 1, filter, backward);

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
