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
#include "internal.h"

/*
 * PUBLIC METHODS
 */

SEXP dots(SEXP env)
{
  if(!Rf_isEnvironment(env))
  {
    Rf_errorcall(R_NilValue, "argument 'env' must be an environment");
  }

  SEXP args = PROTECT(Rf_findVarInFrame(env, R_DotsSymbol));

  if(TYPEOF(args) != DOTSXP)
  {
    UNPROTECT(1);

    return R_NilValue;
  }

  R_len_t n = 0;

  for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
  {
    SEXP arg_car = CAR(arg);

    if(TYPEOF(arg_car) == PROMSXP)
    {
      SETCAR(arg, Rf_eval(arg_car, env));
    }

    n++;
  }

  SEXP dots = PROTECT(Rf_allocVector(VECSXP, n));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SET_VECTOR_ELT(dots, i, CAR(arg));

    arg = CDR(arg);
  }

  UNPROTECT(2);

  return dots;
}

SEXP arg_list(SEXP args)
{
  R_len_t n = 0;

  args = CDR(args);

  for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
  {
    SEXP arg_car = CAR(arg);

    if(TYPEOF(arg_car) == LISTSXP)
    {
      for(SEXP arg_item = arg_car; arg_item != R_NilValue; arg_item = CDR(arg_item))
      {
        n++;
      }
    }
    else
    {
      n++;
    }
  }

  if(n == 0)
  {
    return R_NilValue;
  }

  SEXP list = PROTECT(Rf_allocVector(LISTSXP, n));

  SEXP list_item = list;

  for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
  {
    SEXP arg_car = CAR(arg);

    if(TYPEOF(arg_car) == LISTSXP)
    {
      for(SEXP arg_item = arg_car; arg_item != R_NilValue; arg_item = CDR(arg_item))
      {
        SETCAR(list_item, CAR(arg_item));

        SEXP arg_item_tag = (TAG)(arg_item);

        if(arg_item_tag != R_NilValue)
        {
          SET_TAG(list_item, arg_item_tag);
        }

        list_item = CDR(list_item);
      }
    }
    else
    {
      SETCAR(list_item, arg_car);

      SEXP arg_tag = (TAG)(arg);

      if(arg_tag != R_NilValue)
      {
        SET_TAG(list_item, arg_tag);
      }

      list_item = CDR(list_item);
    }
  }

  UNPROTECT(1);

  return list;
}

SEXP bsum(SEXP x, SEXP block_size)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(block_size))
  {
    Rf_errorcall(R_NilValue, "argument 'block_size' must be a numerical scalar");
  }

  int m = Rf_asInteger(block_size);

  if(m < 0)
  {
    Rf_errorcall(R_NilValue, "invalid block size");
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, m));

  double *po = REAL(out);

  memset(po, 0, m * sizeof(double));

  R_len_t n = XLENGTH(x);

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      double *px = REAL(x);

      for(int i = 0, j = 0; i < n; i++,
          j = (++j == m) ? 0 : j)
      {
        po[j] += px[i];
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *px = INTEGER(x);

      for(int i = 0, j = 0; i < n; i++,
          j = (++j == m) ? 0 : j)
      {
        po[j] += px[i];
      }

      break;
    }
  }

  UNPROTECT(1);

  return out;
}

SEXP approx_gradient(SEXP graph, SEXP target, SEXP node, SEXP index, SEXP epsilon)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(target, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a cg_node object");
  }

  if(!Rf_isNumeric(index))
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be a numeric scalar");
  }

  if(!Rf_isNumeric(epsilon))
  {
    Rf_errorcall(R_NilValue, "argument 'epsilon' must be a numeric scalar");
  }

  int target_index;

  SEXP target_value = R_NilValue;

  PROTECT_WITH_INDEX(target_value = cg_node_value(target), &target_index);

  if(!Rf_isReal(target_value))
  {
    REPROTECT(target_value = Rf_coerceVector(target_value, REALSXP), target_index);

    cg_node_set_value(target, target_value);
  }

  int k = Rf_asInteger(index);

  if(k < 1 || k > XLENGTH(target_value))
  {
    Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at index %d",
                 cg_node_name(target), k);
  }

  int node_index;

  SEXP node_value = R_NilValue;

  PROTECT_WITH_INDEX(node_value = cg_node_value(node), &node_index);

  if(!Rf_isReal(node_value))
  {
    REPROTECT(node_value = Rf_coerceVector(node_value, REALSXP), node_index);

    cg_node_set_value(node, node_value);
  }

  int n = XLENGTH(node_value);

  SEXP grad = PROTECT(Rf_allocVector(REALSXP, n));

  double *x = REAL(node_value);
  double *y = REAL(grad);

  double eps = Rf_asReal(epsilon);

  for(int i = 0; i < n; i++)
  {
    x[i] += eps;

    cg_graph_forward(graph, target);

    REPROTECT(target_value = cg_node_value(target), target_index);

    double t1 = REAL(target_value)[k - 1];

    x[i] -= 2 * eps;

    cg_graph_forward(graph, target);

    REPROTECT(target_value = cg_node_value(target), target_index);

    double t2 = REAL(target_value)[k - 1];

    x[i] += eps;

    y[i] = (t1 - t2) / (2 * eps);
  }

  SHALLOW_DUPLICATE_ATTRIB(grad, node_value);

  cg_graph_forward(graph, target);

  UNPROTECT(3);

  return grad;
}
