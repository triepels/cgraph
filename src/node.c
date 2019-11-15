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
#include "graph.h"
#include "class.h"
#include "session.h"
#include "function.h"

/*
 * SYMBOLS
 */

#define CG_ID_SYMBOL Rf_install("id")
#define CG_TYPE_SYMBOL Rf_install("type")
#define CG_NAME_SYMBOL Rf_install("name")
#define CG_INPUTS_SYMBOL Rf_install("inputs")
#define CG_OUTPUTS_SYMBOL Rf_install("outputs")
#define CG_VALUE_SYMBOL Rf_install("value")
#define CG_FUN_SYMBOL Rf_install("fun")

/*
 * METHODS
 */

const char* cg_node_name(SEXP node)
{
  SEXP name = Rf_findVarInFrame(node, CG_NAME_SYMBOL);

  if(!Rf_isString(name) || Rf_xlength(name) == 0)
  {
    Rf_errorcall(R_NilValue, "node has no name");
  }

  return CHAR(STRING_ELT(name, 0));
}

void cg_node_set_name(SEXP node, const char *name)
{
  if(strcmp(name, "") == 0)
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a non-blank character scalar");
  }

  SEXP node_name = PROTECT(Rf_mkString(name));

  Rf_defineVar(CG_NAME_SYMBOL, node_name, node);

  UNPROTECT(1);
}

SEXP cg_node_symbol(SEXP node)
{
  return Rf_install(cg_node_name(node));
}

int cg_node_id(SEXP node)
{
  SEXP id = Rf_findVarInFrame(node, CG_ID_SYMBOL);

  if(!Rf_isInteger(id) || Rf_xlength(id) < 1)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no id", cg_node_name(node));
  }

  return INTEGER(id)[0];
}

void cg_node_set_id(SEXP node, const int id)
{
  if(id < 1)
  {
    Rf_errorcall(R_NilValue, "argument 'id' must be higher than or equal to 1");
  }

  SEXP node_id = PROTECT(Rf_ScalarInteger(id));

  Rf_defineVar(CG_ID_SYMBOL, node_id, node);

  UNPROTECT(1);
}

int cg_node_type(SEXP node)
{
  SEXP type = Rf_findVarInFrame(node, CG_TYPE_SYMBOL);

  if(!Rf_isInteger(type) || Rf_xlength(type) < 1)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no type", cg_node_name(node));
  }

  return INTEGER(type)[0];
}

void cg_node_set_type(SEXP node, const int type)
{
  if(type < 0 || type > 3)
  {
    Rf_errorcall(R_NilValue, "argument 'type' must be a valid type");
  }

  SEXP node_type = PROTECT(Rf_ScalarInteger(type));

  Rf_defineVar(CG_TYPE_SYMBOL, node_type, node);

  UNPROTECT(1);
}

SEXP cg_node_inputs(SEXP node)
{
  SEXP inputs = PROTECT(Rf_findVarInFrame(node, CG_INPUTS_SYMBOL));

  if(TYPEOF(inputs) != VECSXP)
  {
    UNPROTECT(1);

    return R_NilValue;
  }

  UNPROTECT(1);

  return inputs;
}

void cg_node_add_input(SEXP node, SEXP input)
{
  if(!cg_is(input, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'input' must be a cg_node object");
  }

  int index;

  SEXP inputs = R_NilValue;

  PROTECT_WITH_INDEX(inputs = Rf_findVarInFrame(node, CG_INPUTS_SYMBOL), &index);

  if(TYPEOF(inputs) != VECSXP)
  {
    REPROTECT(inputs = Rf_allocVector(VECSXP, 1), index);

    SET_VECTOR_ELT(inputs, 0, input);
  }
  else
  {
    R_len_t n = Rf_xlength(inputs);

    REPROTECT(inputs = Rf_lengthgets(inputs, n + 1), index);

    SET_VECTOR_ELT(inputs, n, input);
  }

  Rf_defineVar(CG_INPUTS_SYMBOL, inputs, node);

  UNPROTECT(1);
}

SEXP cg_node_value(SEXP node)
{
  SEXP value = Rf_findVarInFrame(node, CG_VALUE_SYMBOL);

  if(value == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no value", cg_node_name(node));
  }

  return value;
}

void cg_node_set_value(SEXP node, SEXP value)
{
  Rf_defineVar(CG_VALUE_SYMBOL, value, node);
}

SEXP cg_node_function(SEXP node)
{
  SEXP function = Rf_findVarInFrame(node, CG_FUN_SYMBOL);

  if(!cg_is(function, "cg_function"))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no function", cg_node_name(node));
  }

  return function;
}

void cg_node_set_function(SEXP node, SEXP function)
{
  if(!cg_is(function, "cg_function"))
  {
    Rf_errorcall(R_NilValue, "argument 'function' must be a cg_function object");
  }

  Rf_defineVar(CG_FUN_SYMBOL, function, node);
}

void cg_node_eval(SEXP node, SEXP values)
{
  SEXP symbol = cg_node_symbol(node);

  SEXP inputs = PROTECT(cg_node_inputs(node));

  R_len_t n = Rf_xlength(inputs);

  if(n > 0)
  {
    SEXP function = PROTECT(cg_node_function(node));

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

    SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

    for(int i = 0; i < n; i++)
    {
      SEXP input = VECTOR_ELT(inputs, i);

      SEXP input_value = PROTECT(Rf_findVarInFrame(values, cg_node_symbol(input)));

      if(input_value == R_UnboundValue)
      {
        Rf_errorcall(R_NilValue, "node '%s' has no value",
          cg_node_name(input));
      }

      SETCAR(args, input_value);

      args = CDR(args);

      UNPROTECT(1);
    }

    SEXP value = PROTECT(Rf_eval(call, R_EmptyEnv));

    Rf_defineVar(symbol, value, values);

    UNPROTECT(4);
  }
  else
  {
    Rf_defineVar(symbol, cg_node_value(node), values);
  }

  UNPROTECT(1);
}

void cg_node_eval_gradients(SEXP node, SEXP values, SEXP gradients)
{
  SEXP symbol = cg_node_symbol(node);

  SEXP value = PROTECT(Rf_findVarInFrame(values, symbol));

  if(value == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no value",
                 cg_node_name(node));
  }

  SEXP grad = PROTECT(Rf_findVarInFrame(gradients, symbol));

  if(grad == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no gradient",
                 cg_node_name(node));
  }

  SEXP inputs = PROTECT(cg_node_inputs(node));

  R_len_t n = Rf_xlength(inputs);

  if(n > 0)
  {
    SEXP function = PROTECT(cg_node_function(node));

    SEXP function_grads = PROTECT(cg_function_grads(function));

    if(Rf_xlength(function_grads) != n)
    {
      Rf_errorcall(R_NilValue, "invalid number of inputs (%d) provided to node '%s'",
                   Rf_xlength(function_grads), cg_node_name(node));
    }

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, n + 2));

    SEXP arg = args;

    for(int i = 0; i < n; i++)
    {
      SEXP input = VECTOR_ELT(inputs, i);

      SEXP input_value = PROTECT(Rf_findVarInFrame(values, cg_node_symbol(input)));

      if(input_value == R_UnboundValue)
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

      SEXP input_grad = PROTECT(Rf_findVarInFrame(gradients, cg_node_symbol(input)));

      SEXP call = PROTECT(Rf_lcons(VECTOR_ELT(function_grads, i), args));

      SEXP result = PROTECT(Rf_eval(call, R_EmptyEnv));

      if(!(Rf_isLogical(result) || Rf_isNumeric(result)))
      {
        Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                     Rf_type2char(TYPEOF(result)), cg_node_name(node));
      }

      if(input_grad == R_UnboundValue)
      {
        Rf_defineVar(cg_node_symbol(input), result, gradients);
      }
      else
      {
        if(TYPEOF(result) != TYPEOF(input_grad))
        {
          Rf_errorcall(R_NilValue, "cannot accumulate gradients of type '%s' and '%s' for node '%s'",
                       Rf_type2char(TYPEOF(input_grad)), Rf_type2char(TYPEOF(result)), cg_node_name(input));
        }

        R_len_t m = Rf_xlength(input_grad);

        if(Rf_xlength(result) != m)
        {
          Rf_errorcall(R_NilValue, "cannot accumulate gradients of length %d and %d for node '%s'",
                       m, Rf_xlength(result), cg_node_name(node));
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

    UNPROTECT(3);
  }

  UNPROTECT(3);
}

/*
 * CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, SEXP name)
{
  SEXP graph = PROTECT(cg_session_graph());

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
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

  cg_node_set_value(node, value);

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
    Rf_errorcall(R_NilValue, "argument 'inputs' must be a list");
  }

  if(!Rf_isNull(name) && !Rf_isValidString(name))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
  }

  R_xlen_t n = Rf_xlength(inputs);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(!cg_is(input, "cg_node"))
    {
      input = PROTECT(cg_constant(input, R_NilValue));

      SET_VECTOR_ELT(inputs, i, input);

      UNPROTECT(1);
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

  cg_node_set_function(node, function);

  for(int i = 0; i < n; i++)
  {
    cg_node_add_input(node, VECTOR_ELT(inputs, i));
  }

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}
