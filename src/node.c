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
#include "session.h"
#include "function.h"

/*
 * INLINED GET/SET FUNCTIONS
 */

extern inline const char* cg_node_name(SEXP node);

extern inline void cg_node_set_name(SEXP node, const char *name);

extern inline int cg_node_id(SEXP node);

extern inline void cg_node_set_id(SEXP node, const int id);

extern inline cg_node_type_t cg_node_type(SEXP node);

extern inline void cg_node_set_type(SEXP node, const cg_node_type_t type);

extern inline SEXP cg_node_inputs(SEXP node);

extern inline void cg_node_set_inputs(SEXP node, SEXP inputs);

extern inline SEXP cg_node_value(SEXP node);

extern inline void cg_node_set_value(SEXP node, SEXP value);

extern inline SEXP cg_node_grad(SEXP node);

extern inline void cg_node_set_grad(SEXP node, SEXP grad);

extern inline SEXP cg_node_function(SEXP node);

extern inline void cg_node_set_function(SEXP node, SEXP function);

/*
 * PUBLIC FUNCTIONS
 */

void cg_node_zero_grad(SEXP node)
{
  SEXP value = PROTECT(cg_node_value(node));

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(value)), cg_node_name(node));
  }

  SEXP grad;

  int index_grad;

  PROTECT_WITH_INDEX(grad = cg_node_grad(node), &index_grad);

  R_len_t n = XLENGTH(value);

  if(!Rf_isReal(grad) || XLENGTH(grad) != n)
  {
    REPROTECT(grad = Rf_allocVector(REALSXP, n), index_grad);
  }

  memset(REAL(grad), 0, n * sizeof(double));

  SHALLOW_DUPLICATE_ATTRIB(grad, value);

  CG_SET(node, CG_GRAD_SYMBOL, grad);

  UNPROTECT(2);
}

void cg_node_fill_grad(SEXP node, SEXP index, const double x)
{
  SEXP value = PROTECT(cg_node_value(node));

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(value)), cg_node_name(node));
  }

  SEXP grad;

  int index_grad;

  PROTECT_WITH_INDEX(grad = cg_node_grad(node), &index_grad);

  R_len_t n = XLENGTH(value);

  if(!Rf_isReal(grad) || XLENGTH(grad) != n)
  {
    REPROTECT(grad = Rf_allocVector(REALSXP, n), index_grad);
  }

  double *pg = REAL(grad);

  if(!Rf_isNull(index))
  {
    int k = Rf_asInteger(index);

    if(k < 1 || k > n)
    {
      Rf_errorcall(R_NilValue, "argument 'index' out of bounds");
    }

    memset(pg, 0, n * sizeof(double));

    pg[k - 1] = x;
  }
  else
  {
    for(int i = 0; i < n; i++)
    {
      pg[i] = x;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(grad, value);

  CG_SET(node, CG_GRAD_SYMBOL, grad);

  UNPROTECT(2);
}

void cg_node_forward(SEXP node)
{
  SEXP inputs = PROTECT(cg_node_inputs(node));

  SEXP input_tags = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

  R_len_t n = XLENGTH(inputs);

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SETCAR(arg, cg_node_value(input));

    if(!Rf_isNull(input_tags))
    {
      SEXP input_tag = STRING_ELT(input_tags, i);

      if(CHAR(input_tag)[0] != '\0')
      {
        SET_TAG(arg, Rf_installChar(input_tag));
      }
    }

    arg = CDR(arg);
  }

  SEXP function = PROTECT(cg_node_function(node));

  SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

  SEXP value = PROTECT(Rf_eval(call, R_EmptyEnv));

  CG_SET(node, CG_VALUE_SYMBOL, value);

  UNPROTECT(6);
}

void cg_node_backward(SEXP node)
{
  SEXP inputs = PROTECT(cg_node_inputs(node));

  SEXP input_tags = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

  R_len_t n = XLENGTH(inputs);

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n + 2));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SETCAR(arg, cg_node_value(input));

    if(!Rf_isNull(input_tags))
    {
      SEXP input_tag = STRING_ELT(input_tags, i);

      if(CHAR(input_tag)[0] != '\0')
      {
        SET_TAG(arg, Rf_installChar(input_tag));
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

  SEXP function_grad_tags = PROTECT(Rf_getAttrib(function_grads, R_NamesSymbol));

  R_len_t m = XLENGTH(function_grads);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(cg_node_type(input) == CGCST)
    {
      continue;
    }

    SEXP function_grad;

    if(Rf_isNull(input_tags))
    {
      if(i >= m)
      {
        Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at input %d",
                     cg_node_name(node), i + 1);
      }

      function_grad = VECTOR_ELT(function_grads, i);
    }
    else
    {
      SEXP input_tag = STRING_ELT(input_tags, i);

      if(CHAR(input_tag)[0] == '\0')
      {
        Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at input %d",
                     cg_node_name(node), i + 1);
      }

      int match = 0;

      if(!Rf_isNull(function_grad_tags))
      {
        for(int j = 0; j < m; j++)
        {
          if(input_tag == STRING_ELT(function_grad_tags, j))
          {
            function_grad = VECTOR_ELT(function_grads, j);

            match = 1;

            break;
          }
        }
      }

      if(!match)
      {
        Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at input '%s'",
                     cg_node_name(node), CHAR(input_tag));
      }
    }

    if(!Rf_isFunction(function_grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient function of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(function_grad)), cg_node_name(node));
    }

    SEXP call = PROTECT(Rf_lcons(function_grad, args));

    SEXP grad = PROTECT(Rf_eval(call, R_EmptyEnv));

    if(!Rf_isReal(grad))
    {
      Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(node));
    }

    SEXP input_grad = PROTECT(cg_node_grad(input));

    R_len_t l = XLENGTH(input_grad);

    if(l != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot accumulate gradients of length %d and %d for node '%s'",
                   l, XLENGTH(grad), cg_node_name(node));
    }

    double *pg = REAL(grad);
    double *pi = REAL(input_grad);

    for(int k = 0; k < l; k++)
    {
      pi[k] += pg[k];
    }

    UNPROTECT(3);
  }

  UNPROTECT(6);
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP node = PROTECT(cg_class("cg_node"));

  if(Rf_isNull(name))
  {
    CG_SET(node, CG_NAME_SYMBOL, cg_graph_gen_name(graph));
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_GRAD_SYMBOL, R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, value);

  CG_SET(node, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGCST));

  CG_SET(node, CG_ID_SYMBOL, R_NilValue);

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}

SEXP cg_parameter(SEXP value, SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP node = PROTECT(cg_class("cg_node"));

  if(Rf_isNull(name))
  {
    CG_SET(node, CG_NAME_SYMBOL, cg_graph_gen_name(graph));
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_GRAD_SYMBOL, R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, Rf_duplicate(value));

  CG_SET(node, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGPRM));

  CG_SET(node, CG_ID_SYMBOL, R_NilValue);

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}

SEXP cg_input(SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP node = PROTECT(cg_class("cg_node"));

  if(Rf_isNull(name))
  {
    CG_SET(node, CG_NAME_SYMBOL, cg_graph_gen_name(graph));
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_GRAD_SYMBOL, R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, R_NilValue);

  CG_SET(node, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGIPT));

  CG_SET(node, CG_ID_SYMBOL, R_NilValue);

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}

SEXP cg_operator(SEXP function, SEXP inputs, SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!cg_is(function, "cg_function"))
  {
    Rf_errorcall(R_NilValue, "argument 'function' must be a cg_function object");
  }

  if(TYPEOF(inputs) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'inputs' must be a list of inputs");
  }

  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  int can_eval = 1;

  R_xlen_t n = XLENGTH(inputs);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(cg_is(input, "cg_node"))
    {
      SEXP value = PROTECT(cg_node_value(input));

      if(Rf_isNull(value))
      {
        can_eval = 0;
      }

      UNPROTECT(1);
    }
    else
    {
      SET_VECTOR_ELT(inputs, i, cg_constant(input, R_NilValue));
    }
  }

  SEXP node = PROTECT(cg_class("cg_node"));

  if(Rf_isNull(name))
  {
    CG_SET(node, CG_NAME_SYMBOL, cg_graph_gen_name(graph));
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_GRAD_SYMBOL, R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, R_NilValue);

  CG_SET(node, CG_FUN_SYMBOL, function);

  CG_SET(node, CG_INPUTS_SYMBOL, inputs);

  CG_SET(node, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGOPR));

  CG_SET(node, CG_ID_SYMBOL, R_NilValue);

  if(cg_graph_eager(graph) && can_eval)
  {
    cg_node_forward(node);
  }

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}
