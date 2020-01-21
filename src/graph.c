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

    } while (cg_graph_node_exists(graph, name));
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
      Rf_errorcall(R_NilValue, "argument 'index' must be between 1 and %d", n);
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

  if(Rf_isNull(inputs))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no inputs",
                 cg_node_name(node));
  }

  R_len_t n = XLENGTH(inputs);

  SEXP function = PROTECT(cg_node_function(node));

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

  SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SEXP input_value = PROTECT(cg_node_value(input));

    if(Rf_isNull(input_value))
    {
      Rf_errorcall(R_NilValue, "node '%s' has no value",
                   cg_node_name(input));
    }

    SETCAR(args, input_value);

    args = CDR(args);

    UNPROTECT(1);
  }

  SEXP result = PROTECT(Rf_eval(call, R_EmptyEnv));

  cg_node_set_value(node, result);

  UNPROTECT(5);
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
  SEXP value = PROTECT(cg_node_value(node));

  if(Rf_isNull(value))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no value",
                 Rf_type2char(TYPEOF(value)));
  }

  SEXP grad = PROTECT(cg_node_grad(node));

  if(Rf_isNull(grad))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no gradient",
                 cg_node_name(node));
  }

  SEXP inputs = PROTECT(cg_node_inputs(node));

  if(Rf_isNull(inputs))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no inputs",
                 cg_node_name(node));
  }

  R_len_t n = XLENGTH(inputs);

  SEXP function = PROTECT(cg_node_function(node));

  SEXP function_grads = PROTECT(cg_function_grads(function));

  if(n != XLENGTH(function_grads))
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid number of inputs (%d)",
                 cg_node_name(node), n);
  }

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n + 2));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SEXP input_value = PROTECT(cg_node_value(input));

    if(Rf_isNull(input_value))
    {
      Rf_errorcall(R_NilValue, "node '%s' has no value",
                   cg_node_name(input));
    }

    SETCAR(arg, input_value);

    arg = CDR(arg);

    UNPROTECT(1);
  }

  SET_TAG(arg, Rf_install("val"));

  SETCAR(arg, value);

  SET_TAG(CDR(arg), Rf_install("grad"));

  SETCADR(arg, grad);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(cg_node_type(input) == CGCST)
    {
      continue;
    }

    SEXP call = PROTECT(Rf_lcons(VECTOR_ELT(function_grads, i), args));

    SEXP result = PROTECT(Rf_eval(call, R_EmptyEnv));

    if(!(Rf_isLogical(result) || Rf_isNumeric(result)))
    {
      Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(result)), cg_node_name(node));
    }

    SEXP input_grad = PROTECT(cg_node_grad(input));

    if(Rf_isNull(input_grad))
    {
      cg_node_set_grad(input, result);
    }
    else
    {
      if(TYPEOF(result) != TYPEOF(input_grad))
      {
        Rf_errorcall(R_NilValue, "cannot accumulate gradients of type '%s' and '%s' for node '%s'",
                     Rf_type2char(TYPEOF(input_grad)), Rf_type2char(TYPEOF(result)), cg_node_name(input));
      }

      R_len_t m = XLENGTH(input_grad);

      if(XLENGTH(result) != m)
      {
        Rf_errorcall(R_NilValue, "cannot accumulate gradients of length %d and %d for node '%s'",
                     XLENGTH(result), m, cg_node_name(node));
      }

      switch(TYPEOF(result))
      {
        case REALSXP :
        {
          double *x = REAL(result);
          double *y = REAL(input_grad);

          for(int j = 0; j < m; j++)
          {
            y[j] += x[j];
          }

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *x = INTEGER(result);
          int *y = INTEGER(input_grad);

          for(int j = 0; j < m; j++)
          {
            y[j] += x[j];
          }

          break;
        }
      }
    }

    UNPROTECT(3);
  }

  UNPROTECT(6);
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

SEXP cg_graph()
{
  SEXP graph = PROTECT(cg_class1("cg_graph"));

  cg_session_set_graph(graph);

  UNPROTECT(1);

  return graph;
}
