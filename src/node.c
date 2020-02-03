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
#include "class.h"
#include "session.h"
#include "function.h"

/*
 * METHODS
 */

const char* cg_node_name(SEXP node)
{
  SEXP name = PROTECT(CG_GET(node, CG_NAME_SYMBOL));

  if(!IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "node has no name");
  }

  UNPROTECT(1);

  return CHAR(STRING_ELT(name, 0));
}

void cg_node_set_name(SEXP node, const char *name)
{
  if(strcmp(name, "") == 0)
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a non-blank character scalar");
  }

  CG_SET(node, CG_NAME_SYMBOL, Rf_mkString(name));
}

int cg_node_id(SEXP node)
{
  SEXP id = PROTECT(CG_GET(node, CG_ID_SYMBOL));

  if(!IS_SCALAR(id, INTSXP))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no id", cg_node_name(node));
  }

  UNPROTECT(1);

  return INTEGER(id)[0];
}

void cg_node_set_id(SEXP node, const int id)
{
  if(id < 1)
  {
    Rf_errorcall(R_NilValue, "argument 'id' must be a positive integer");
  }

  CG_SET(node, CG_ID_SYMBOL, Rf_ScalarInteger(id));
}

cg_node_type_t cg_node_type(SEXP node)
{
  SEXP type = PROTECT(CG_GET(node, CG_TYPE_SYMBOL));

  if(!IS_SCALAR(type, INTSXP))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no type", cg_node_name(node));
  }

  UNPROTECT(1);

  return (cg_node_type_t)INTEGER(type)[0];
}

void cg_node_set_type(SEXP node, const cg_node_type_t type)
{
  CG_SET(node, CG_TYPE_SYMBOL, Rf_ScalarInteger(type));
}

SEXP cg_node_inputs(SEXP node)
{
  SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

  if(TYPEOF(inputs) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no inputs", cg_node_name(node));
  }

  UNPROTECT(1);

  return inputs;
}

void cg_node_set_inputs(SEXP node, SEXP inputs)
{
  if(TYPEOF(inputs) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'inputs' must be a list of inputs");
  }

  CG_SET(node, CG_INPUTS_SYMBOL, inputs);
}

SEXP cg_node_value(SEXP node)
{
  SEXP value = PROTECT(CG_GET(node, CG_VALUE_SYMBOL));

  if(value == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no value", cg_node_name(node));
  }

  UNPROTECT(1);

  return value;
}

void cg_node_set_value(SEXP node, SEXP value)
{
  CG_SET(node, CG_VALUE_SYMBOL, value);
}

SEXP cg_node_grad(SEXP node)
{
  SEXP grad = PROTECT(CG_GET(node, CG_GRAD_SYMBOL));

  if(grad == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no gradient", cg_node_name(node));
  }

  UNPROTECT(1);

  return grad;
}

void cg_node_set_grad(SEXP node, SEXP grad)
{
  CG_SET(node, CG_GRAD_SYMBOL, grad);
}

SEXP cg_node_function(SEXP node)
{
  SEXP function = PROTECT(CG_GET(node, CG_FUN_SYMBOL));

  if(!cg_is(function, "cg_function"))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no function", cg_node_name(node));
  }

  UNPROTECT(1);

  return function;
}

void cg_node_set_function(SEXP node, SEXP function)
{
  if(!cg_is(function, "cg_function"))
  {
    Rf_errorcall(R_NilValue, "argument 'function' must be a cg_function object");
  }

  CG_SET(node, CG_FUN_SYMBOL, function);
}

void cg_node_forward(SEXP node)
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

void cg_node_backward(SEXP node)
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
      Rf_errorcall(R_NilValue, "unable to accumulate gradient of type '%s' for node '%s'",
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
        Rf_errorcall(R_NilValue, "unable to accumulate gradients of length %d and %d for node '%s'",
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

/*
 * CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && TYPEOF(name) != STRSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP node = PROTECT(cg_class1("cg_node"));

  cg_node_set_type(node, CGCST);

  if(Rf_isNull(name))
  {
    cg_node_set_name(node, cg_graph_gen_name(graph));
  }
  else
  {
    cg_node_set_name(node, CHAR(STRING_ELT(name, 0)));
  }

  CG_SET(node, CG_VALUE_SYMBOL, value);
  CG_SET(node, CG_GRAD_SYMBOL, R_NilValue);

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}

SEXP cg_parameter(SEXP value, SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  SEXP node = PROTECT(cg_class1("cg_node"));

  cg_node_set_type(node, CGPRM);

  if(Rf_isNull(name))
  {
    cg_node_set_name(node, cg_graph_gen_name(graph));
  }
  else
  {
    cg_node_set_name(node, CHAR(STRING_ELT(name, 0)));
  }

  cg_node_set_value(node, value);
  cg_node_set_grad(node, R_NilValue);

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}

SEXP cg_input(SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  SEXP node = PROTECT(cg_class1("cg_node"));

  cg_node_set_type(node, CGIPT);

  if(Rf_isNull(name))
  {
    cg_node_set_name(node, cg_graph_gen_name(graph));
  }
  else
  {
    cg_node_set_name(node, CHAR(STRING_ELT(name, 0)));
  }

  cg_node_set_value(node, R_NilValue);
  cg_node_set_grad(node, R_NilValue);

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

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
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

  SEXP node = PROTECT(cg_class1("cg_node"));

  cg_node_set_type(node, CGOPR);

  if(Rf_isNull(name))
  {
    cg_node_set_name(node, cg_graph_gen_name(graph));
  }
  else
  {
    cg_node_set_name(node, CHAR(STRING_ELT(name, 0)));
  }

  cg_node_set_inputs(node, inputs);
  cg_node_set_function(node, function);

  if(cg_graph_eager(graph) && can_eval)
  {
    cg_node_forward(node);
  }
  else
  {
    cg_node_set_value(node, R_NilValue);
  }

  cg_node_set_grad(node, R_NilValue);

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}
