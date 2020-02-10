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

void cg_node_forward(SEXP node)
{
  SEXP inputs = PROTECT(cg_node_inputs(node));

  SEXP args = PROTECT(Rf_duplicate(inputs));

  for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
  {
    SETCAR(arg, cg_node_value(CAR(arg)));
  }

  SEXP function = PROTECT(cg_node_function(node));

  SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

  SEXP value = PROTECT(Rf_eval(call, R_EmptyEnv));

  CG_SET(node, CG_VALUE_SYMBOL, value);

  UNPROTECT(5);
}

void cg_node_backward(SEXP node)
{
  SEXP inputs = PROTECT(cg_node_inputs(node));

  SEXP args = PROTECT(Rf_duplicate(inputs));

  for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
  {
    SETCAR(arg, cg_node_value(CAR(arg)));
  }

///CHANGE ME!


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
      Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at input %d",
                   cg_node_name(node), i + 1);
    }

    SEXP function_grad = VECTOR_ELT(function_grads, i);

    if(!Rf_isFunction(function_grad))
    {
      Rf_errorcall(R_NilValue, "function has an invalid gradient at index %d", i + 1);
    }

    SEXP call = PROTECT(Rf_lcons(function_grad, args));

    SEXP grad = PROTECT(Rf_eval(call, R_EmptyEnv));

    if(!Rf_isNumeric(grad))
    {
      Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(node));
    }

    SEXP input_grad = PROTECT(cg_node_grad(input));

    if(Rf_isNull(input_grad))
    {
      CG_SET(input, CG_GRAD_SYMBOL, grad);
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
          double *pi = REAL(input_grad);

          switch(TYPEOF(grad))
          {
            case REALSXP :
            {
              double *pg = REAL(grad);

              for(int j = 0; j < m; j++)
              {
                pi[j] += pg[j];
              }

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *pg = INTEGER(grad);

              for(int j = 0; j < m; j++)
              {
                pi[j] += pg[j];
              }

              break;
            }
          }

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *pi = INTEGER(input_grad);

          switch(TYPEOF(grad))
          {
            case REALSXP :
            {
              double *pg = REAL(grad);

              for(int j = 0; j < m; j++)
              {
                pi[j] += pg[j];
              }

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *pg = INTEGER(grad);

              for(int j = 0; j < m; j++)
              {
                pi[j] += pg[j];
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

  CG_SET(node, CG_GRAD_SYMBOL,  R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, value);

  CG_SET(node, CG_TYPE_SYMBOL,  Rf_ScalarInteger(CGCST));

  CG_SET(node, CG_ID_SYMBOL,    R_NilValue);

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

  CG_SET(node, CG_GRAD_SYMBOL,  R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, value);

  CG_SET(node, CG_TYPE_SYMBOL,  Rf_ScalarInteger(CGPRM));

  CG_SET(node, CG_ID_SYMBOL,    R_NilValue);

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

  CG_SET(node, CG_GRAD_SYMBOL,  R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL, R_NilValue);

  CG_SET(node, CG_TYPE_SYMBOL,  Rf_ScalarInteger(CGIPT));

  CG_SET(node, CG_ID_SYMBOL,    R_NilValue);

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

  if(TYPEOF(inputs) != LISTSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'inputs' must be a pairlist of inputs");
  }

  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  int can_eval = 1;

  for(SEXP input = inputs; input != R_NilValue; input = CDR(input))
  {
    SEXP input_car = CAR(input);

    if(cg_is(input_car, "cg_node"))
    {
      SEXP value = PROTECT(cg_node_value(input_car));

      if(Rf_isNull(value))
      {
        can_eval = 0;
      }

      UNPROTECT(1);
    }
    else
    {
      SETCAR(input, cg_constant(input_car, R_NilValue));
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

  CG_SET(node, CG_GRAD_SYMBOL,   R_NilValue);

  CG_SET(node, CG_VALUE_SYMBOL,  R_NilValue);

  CG_SET(node, CG_FUN_SYMBOL,    function);

  CG_SET(node, CG_INPUTS_SYMBOL, inputs);

  CG_SET(node, CG_TYPE_SYMBOL,   Rf_ScalarInteger(CGOPR));

  CG_SET(node, CG_ID_SYMBOL,     R_NilValue);

  if(cg_graph_eager(graph) && can_eval)
  {
    cg_node_forward(node);
  }

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}
