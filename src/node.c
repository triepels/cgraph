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
 * SYMBOLS
 */

#define CG_ID_SYMBOL Rf_install("id")
#define CG_TYPE_SYMBOL Rf_install("type")
#define CG_NAME_SYMBOL Rf_install("name")
#define CG_INPUTS_SYMBOL Rf_install("inputs")
#define CG_VALUE_SYMBOL Rf_install("value")
#define CG_GRAD_SYMBOL Rf_install("grad")
#define CG_FUN_SYMBOL Rf_install("fun")

/*
 * METHODS
 */

const char* cg_node_name(SEXP node)
{
  SEXP name = Rf_findVarInFrame(node, CG_NAME_SYMBOL);

  if(!IS_SCALAR(name, STRSXP))
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

int cg_node_id(SEXP node)
{
  SEXP id = Rf_findVarInFrame(node, CG_ID_SYMBOL);

  if(!IS_SCALAR(id, INTSXP))
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

  if(!IS_SCALAR(type, INTSXP))
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

void cg_node_set_inputs(SEXP node, SEXP inputs)
{
  if(TYPEOF(inputs) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'inputs' must be a list of inputs");
  }

  R_len_t n = XLENGTH(inputs);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(!cg_is(input, "cg_node"))
    {
      Rf_errorcall(R_NilValue, "invalid input provided to argument 'inputs' at index %d", i + 1);
    }
  }

  Rf_defineVar(CG_INPUTS_SYMBOL, inputs, node);
}

SEXP cg_node_value(SEXP node)
{
  SEXP value = PROTECT(Rf_findVarInFrame(node, CG_VALUE_SYMBOL));

  if(value == R_UnboundValue)
  {
    UNPROTECT(1);

    return R_NilValue;
  }

  UNPROTECT(1);

  return value;
}

void cg_node_set_value(SEXP node, SEXP value)
{
  Rf_defineVar(CG_VALUE_SYMBOL, value, node);
}

SEXP cg_node_grad(SEXP node)
{
  SEXP grad = PROTECT(Rf_findVarInFrame(node, CG_GRAD_SYMBOL));

  if(grad == R_UnboundValue)
  {
    UNPROTECT(1);

    return R_NilValue;
  }

  UNPROTECT(1);

  return grad;
}

void cg_node_set_grad(SEXP node, SEXP grad)
{
  Rf_defineVar(CG_GRAD_SYMBOL, grad, node);
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

  cg_node_set_value(node, R_NilValue);

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
    }
    else
    {
      SEXP node = PROTECT(cg_constant(input, R_NilValue));

      SET_VECTOR_ELT(inputs, i, node);
    }

    UNPROTECT(1);
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

  cg_node_set_inputs(node, inputs);

  if(cg_graph_eager(graph) && can_eval)
  {
    SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

    SEXP names = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

    SEXP arg = args;

    for(int i = 0; i < n; i++)
    {
      SEXP input = VECTOR_ELT(inputs, i);

      SETCAR(arg, cg_node_value(input));

      if(names != R_NilValue && CHAR(STRING_ELT(names, i))[0] != '\0')
      {
        SET_TAG(arg, Rf_installTrChar(STRING_ELT(names, i)));
      }

      arg = CDR(arg);
    }

    SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

    SEXP result = PROTECT(Rf_eval(call, R_EmptyEnv));

    cg_node_set_value(node, result);

    UNPROTECT(4);
  }
  else
  {
    cg_node_set_value(node, R_NilValue);
  }

  cg_graph_add_node(graph, node);

  UNPROTECT(2);

  return node;
}
