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
 * CLASS DEFINITIONS
 */

static const cg_class_def_t CST_DEF[] = {
  { "id",     1 },
  { "type",   1 },
  { "name",   1 },
  { "value",  1 },
  { "grad",   0 },
  { NULL,     0 }
};

static const cg_class_def_t PRM_DEF[] = {
  { "id",     1 },
  { "type",   1 },
  { "name",   1 },
  { "value",  0 },
  { "grad",   0 },
  { NULL,     0 }
};


static const cg_class_def_t IPT_DEF[] = {
  { "id",     1 },
  { "type",   1 },
  { "name",   1 },
  { "value",  0 },
  { "grad",   0 },
  { NULL,     0 }
};

static const cg_class_def_t OPR_DEF[] = {
  { "id",     1 },
  { "type",   1 },
  { "name",   1 },
  { "inputs", 1 },
  { "fun",    1 },
  { "value",  0 },
  { "grad",   0 },
  { NULL,     0 }
};

/*
 * METHODS
 */

void cg_node_gen_name(SEXP node, SEXP graph)
{
  char *name = R_alloc(1, 32 * sizeof(char));

  SEXP nodes = PROTECT(CG_GET(graph, CG_NODES_SYMBOL));

  if(TYPEOF(nodes) != VECSXP)
  {
    strcpy(name, "v1");
  }
  else
  {
    R_len_t n = XLENGTH(nodes);

    sprintf(name, "v%d", n + 1);
  }

  CG_SET_STR(node, CG_NAME_SYMBOL, name);

  UNPROTECT(1);
}

void cg_node_forward(SEXP node)
{
  SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

  R_len_t n = XLENGTH(inputs);

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

  SEXP names = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SETCAR(arg, CG_GET(input, CG_VALUE_SYMBOL));

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

  SEXP function = PROTECT(CG_GET(node, CG_FUN_SYMBOL));

  SEXP call = PROTECT(Rf_lcons(CG_GET(function, CG_DEF_SYMBOL), args));

  SEXP value = PROTECT(Rf_eval(call, R_EmptyEnv));

  CG_SET(node, CG_VALUE_SYMBOL, value);

  UNPROTECT(6);
}

void cg_node_backward(SEXP node)
{
  SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

  R_len_t n = XLENGTH(inputs);

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, n + 2));

  SEXP names = PROTECT(Rf_getAttrib(inputs, R_NamesSymbol));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    SETCAR(arg, CG_GET(input, CG_VALUE_SYMBOL));

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

  SETCAR(arg, CG_GET(node, CG_VALUE_SYMBOL));

  SET_TAG(arg, CG_VALUE_SYMBOL);

  SETCADR(arg, CG_GET(node, CG_GRAD_SYMBOL));

  SET_TAG(CDR(arg), CG_GRAD_SYMBOL);

  SEXP function = PROTECT(CG_GET(node, CG_FUN_SYMBOL));

  SEXP function_grads = PROTECT(CG_GET(function, CG_GRADS_SYMBOL));

  R_len_t k = XLENGTH(function_grads);

  for(int i = 0; i < n; i++)
  {
    SEXP input = VECTOR_ELT(inputs, i);

    if(CG_GET_INT(input, CG_TYPE_SYMBOL) == CGCST)
    {
      continue;
    }

    if(i >= k)
    {
      Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at input %d",
                   CG_GET_STR(node, CG_NAME_SYMBOL), i + 1);
    }

    SEXP call = PROTECT(Rf_lcons(VECTOR_ELT(function_grads, i), args));

    SEXP grad = PROTECT(Rf_eval(call, R_EmptyEnv));

    if(!Rf_isNumeric(grad))
    {
      Rf_errorcall(R_NilValue, "cannot accumulate gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), CG_GET_STR(node, CG_NAME_SYMBOL));
    }

    SEXP input_grad = PROTECT(CG_GET(input, CG_GRAD_SYMBOL));

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
                     XLENGTH(grad), m, CG_GET_STR(node, CG_NAME_SYMBOL));
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
 * CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, SEXP name)
{
  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP graph = PROTECT(cg_session_graph());

  SEXP node = PROTECT(cg_class("cg_node", CST_DEF));

  CG_SET_INT(node, CG_TYPE_SYMBOL, CGCST);

  if(Rf_isNull(name))
  {
    cg_node_gen_name(node, graph);
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_VALUE_SYMBOL, value);

  cg_graph_add_node(graph, node);

  cg_class_lock(node, CST_DEF);

  UNPROTECT(2);

  return node;
}

SEXP cg_parameter(SEXP value, SEXP name)
{
  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP graph = PROTECT(cg_session_graph());

  SEXP node = PROTECT(cg_class("cg_node", PRM_DEF));

  CG_SET_INT(node, CG_TYPE_SYMBOL, CGPRM);

  if(Rf_isNull(name))
  {
    cg_node_gen_name(node, graph);
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_VALUE_SYMBOL, value);

  cg_graph_add_node(graph, node);

  cg_class_lock(node, PRM_DEF);

  UNPROTECT(2);

  return node;
}

SEXP cg_input(SEXP name)
{
  if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'name' must be NULL or a character scalar");
  }

  SEXP graph = PROTECT(cg_session_graph());

  SEXP node = PROTECT(cg_class("cg_node", IPT_DEF));

  CG_SET_INT(node, CG_TYPE_SYMBOL, CGIPT);

  if(Rf_isNull(name))
  {
    cg_node_gen_name(node, graph);
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  cg_graph_add_node(graph, node);

  cg_class_lock(node, IPT_DEF);

  UNPROTECT(2);

  return node;
}

SEXP cg_operator(SEXP function, SEXP inputs, SEXP name)
{
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
      SEXP value = PROTECT(CG_GET(input, CG_VALUE_SYMBOL));

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

  SEXP graph = PROTECT(cg_session_graph());

  SEXP node = PROTECT(cg_class("cg_node", OPR_DEF));

  CG_SET_INT(node, CG_TYPE_SYMBOL, CGOPR);

  if(Rf_isNull(name))
  {
    cg_node_gen_name(node, graph);
  }
  else
  {
    CG_SET(node, CG_NAME_SYMBOL, name);
  }

  CG_SET(node, CG_INPUTS_SYMBOL, inputs);
  CG_SET(node, CG_FUN_SYMBOL, function);

  if(CG_GET_LGL(graph, CG_EAGER_SYMBOL) && can_eval)
  {
    cg_node_forward(node);
  }

  cg_graph_add_node(graph, node);

  cg_class_lock(node, OPR_DEF);

  UNPROTECT(2);

  return node;
}
