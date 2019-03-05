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
#include "function.h"

/*
 * SYMBOLS
 */

#define CG_ID_SYMBOL Rf_install("id")
#define CG_NAME_SYMBOL Rf_install("name")
#define CG_INPUTS_SYMBOL Rf_install("inputs")
#define CG_TYPE_SYMBOL Rf_install("type")
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
  SEXP node_name = Rf_findVarInFrame(node, CG_NAME_SYMBOL);

  if(strcmp(name, "") == 0)
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be a non-blank character scalar");
  }

  if(!Rf_isString(node_name))
  {
    Rf_defineVar(CG_NAME_SYMBOL, Rf_mkString(name), node);
  }
  else
  {
    SET_STRING_ELT(node_name, 0, Rf_mkChar(name));
  }
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
  SEXP node_id = Rf_findVarInFrame(node, CG_ID_SYMBOL);

  if(!Rf_isInteger(node_id))
  {
    Rf_defineVar(CG_ID_SYMBOL, Rf_ScalarInteger(id), node);
  }
  else
  {
    INTEGER(node_id)[0] = id;
  }
}

SEXP cg_node_inputs(SEXP node, int unique)
{
  SEXP inputs = Rf_findVarInFrame(node, CG_INPUTS_SYMBOL);

  if(TYPEOF(inputs) != VECSXP)
  {
    return R_NilValue;
  }

  if(!unique)
  {
    return inputs;
  }
  else
  {
    int m = 0;

    R_len_t n = Rf_xlength(inputs);

    SEXP unique_inputs = PROTECT(Rf_allocVector(VECSXP, n));

    for(int i = 0; i < n; i++)
    {
      int found = 0;

      SEXP input = VECTOR_ELT(inputs, i);

      for(int j = 0; j < m; j++)
      {
        if(input == VECTOR_ELT(unique_inputs, j))
        {
          found = 1;

          break;
        }
      }

      if(!found)
      {
        SET_VECTOR_ELT(unique_inputs, m, input);

        m++;
      }
    }

    SETLENGTH(unique_inputs, m);

    UNPROTECT(1);

    return unique_inputs;
  }
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

SEXP cg_node_outputs(SEXP node, int unique)
{
  SEXP outputs = Rf_findVarInFrame(node, CG_OUTPUTS_SYMBOL);

  if(TYPEOF(outputs) != VECSXP)
  {
    return R_NilValue;
  }

  if(!unique)
  {
    return outputs;
  }
  else
  {
    int m = 0;

    R_len_t n = Rf_xlength(outputs);

    SEXP unique_outputs = PROTECT(Rf_allocVector(VECSXP, n));

    for(int i = 0; i < n; i++)
    {
      int found = 0;

      SEXP input = VECTOR_ELT(outputs, i);

      for(int j = 0; j < m; j++)
      {
        if(input == VECTOR_ELT(unique_outputs, j))
        {
          found = 1;

          break;
        }
      }

      if(!found)
      {
        SET_VECTOR_ELT(unique_outputs, m, input);

        m++;
      }
    }

    SETLENGTH(unique_outputs, m);

    UNPROTECT(1);

    return unique_outputs;
  }
}

void cg_node_add_output(SEXP node, SEXP output)
{
  if(!cg_is(output, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'output' must be a cg_node object");
  }

  SEXP outputs = Rf_findVarInFrame(node, CG_OUTPUTS_SYMBOL);

  if(TYPEOF(outputs) != VECSXP)
  {
    outputs = PROTECT(Rf_allocVector(VECSXP, 1));

    SET_VECTOR_ELT(outputs, 0, output);
  }
  else
  {
    R_len_t n = Rf_xlength(outputs);

    outputs = PROTECT(Rf_lengthgets(outputs, n + 1));

    SET_VECTOR_ELT(outputs, n, output);
  }

  Rf_defineVar(CG_OUTPUTS_SYMBOL, outputs, node);

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
  SEXP inputs = cg_node_inputs(node, FALSE);

  if(Rf_xlength(inputs) > 0)
  {
    SEXP function = cg_node_function(node);

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, Rf_xlength(inputs)));

    SEXP call = PROTECT(Rf_lcons(cg_function_def(function), args));

    int i = 0;

    for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
    {
      SETCAR(arg, cg_node_symbol(VECTOR_ELT(inputs, i)));

      i++;
    }

    SEXP value = PROTECT(Rf_eval(call, values));

    Rf_defineVar(cg_node_symbol(node), value, values);

    UNPROTECT(3);
  }
  else
  {
    SEXP value = cg_node_value(node);

    if(!Rf_isNull(value))
    {
      Rf_defineVar(cg_node_symbol(node), cg_node_value(node), values);
    }
  }
}

void cg_node_eval_gradient(SEXP node, SEXP values, SEXP gradients)
{
  int index;

  SEXP gradient;

  PROTECT_WITH_INDEX(gradient = R_NilValue, &index);

  SEXP outputs = PROTECT(cg_node_outputs(node, TRUE));

  R_len_t n, m = Rf_xlength(outputs);

  for(int j = 0; j < m; j++)
  {
    SEXP output = VECTOR_ELT(outputs, j);

    SEXP output_symbol = cg_node_symbol(output);

    SEXP output_gradient = Rf_findVarInFrame(gradients, output_symbol);

    if(output_gradient != R_UnboundValue)
    {
      SEXP output_inputs = cg_node_inputs(output, FALSE);

      SEXP output_function = cg_node_function(output);

      SEXP output_function_grads = cg_function_grads(output_function);

      R_len_t p = Rf_xlength(output_inputs);

      if(p != Rf_xlength(output_function_grads))
      {
        Rf_errorcall(R_NilValue, "invalid number of inputs (%d) provided to node '%s'",
                     p, cg_node_name(output));
      }

      for(int k = 0; k < p; k++)
      {
        SEXP output_input = VECTOR_ELT(output_inputs, k);

        if(node == output_input)
        {
          SEXP output_function_grad = VECTOR_ELT(output_function_grads, k);

          if(!Rf_isFunction(output_function_grad))
          {
            Rf_errorcall(R_NilValue, "node '%s' has an invalid gradient at index %d",
                         cg_node_name(output), k);
          }

          SEXP args = PROTECT(Rf_allocVector(LISTSXP, p + 2));

          SEXP call = PROTECT(Rf_lcons(output_function_grad, args));

          for(int l = 0; l < p; l++)
          {
            SEXP input = VECTOR_ELT(output_inputs, l);

            SETCAR(args, cg_node_symbol(input));

            args = CDR(args);
          }

          SET_TAG(args, Rf_install("val"));

          SETCAR(args, output_symbol);

          SET_TAG(CDR(args), Rf_install("grad"));

          SETCADR(args, output_gradient);

          SEXP result = PROTECT(Rf_eval(call, values));

          if(!(Rf_isLogical(result) || Rf_isNumeric(result)))
          {
            Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                         Rf_type2char(TYPEOF(result)), cg_node_name(node));
          }

          if(Rf_isNull(gradient))
          {
            REPROTECT(gradient = result, index);

            n = Rf_xlength(gradient);
          }
          else
          {
            if(n != Rf_xlength(result))
            {
              Rf_errorcall(R_NilValue, "cannot accumulate gradient of length %d and %d for node '%s'",
                           n, Rf_xlength(result), cg_node_name(node));
            }

            switch(TYPEOF(gradient))
            {
              case REALSXP :
              {
                double *y = REAL(gradient);

                switch(TYPEOF(result))
                {
                  case REALSXP :
                  {
                    double *x = REAL(result);

                    for(int l = 0; l < n; l++)
                    {
                      y[l] += x[l];
                    }

                    break;
                  }
                  case LGLSXP :
                  case INTSXP :
                  {
                    int *x = INTEGER(result);

                    for(int l = 0; l < n; l++)
                    {
                      y[l] += x[l];
                    }

                    break;
                  }
                }

                break;
              }
              case LGLSXP :
              case INTSXP :
              {
                int *y = INTEGER(gradient);

                switch(TYPEOF(result))
                {
                  case REALSXP :
                  {
                    double *x = REAL(result);

                    for(int l = 0; l < n; l++)
                    {
                      y[l] += x[l];
                    }

                    break;
                  }
                  case LGLSXP :
                  case INTSXP :
                  {
                    int *x = INTEGER(result);

                    for(int l = 0; l < n; l++)
                    {
                      y[l] += x[l];
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
      }
    }
  }

  Rf_defineVar(cg_node_symbol(node), gradient, gradients);

  UNPROTECT(2);
}

/*
 * CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, const char *name)
{
  SEXP constant = PROTECT(cg_class2("cg_constant", "cg_node"));

  cg_node_set_name(constant, name);

  cg_node_set_value(constant, value);

  UNPROTECT(1);

  return constant;
}

SEXP cg_parameter(SEXP value, const char *name)
{
  SEXP parameter = PROTECT(cg_class2("cg_parameter", "cg_node"));

  cg_node_set_name(parameter, name);

  cg_node_set_value(parameter, value);

  UNPROTECT(1);

  return parameter;
}

SEXP cg_input(const char *name)
{
  SEXP input = PROTECT(cg_class2("cg_input", "cg_node"));

  cg_node_set_name(input, name);

  UNPROTECT(1);

  return input;
}

SEXP cg_operator(SEXP function, SEXP inputs, const char *name)
{
  SEXP op = PROTECT(cg_class2("cg_operator", "cg_node"));

  cg_node_set_name(op, name);

  cg_node_set_function(op, function);

  R_len_t n = Rf_xlength(inputs);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    cg_node_add_input(op, input);

    cg_node_add_output(input, op);
  }

  UNPROTECT(1);

  return op;
}