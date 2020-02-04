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

    return Rf_allocVector(VECSXP, 0);
  }

  R_len_t n = 0;

  for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
  {
    SEXP arg_value = CAR(arg);

    if(TYPEOF(arg_value) == PROMSXP)
    {
      SETCAR(arg, Rf_eval(arg_value, env));
    }

    n++;
  }

  if(n == 1 && CAR(args) == R_MissingArg)
  {
    UNPROTECT(1);

    return Rf_allocVector(VECSXP, 0);
  }

  SEXP dots = PROTECT(Rf_allocVector(VECSXP, n));

  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));

  SEXP arg = args;

  for(int i = 0; i < n; i++)
  {
    SET_VECTOR_ELT(dots, i, CAR(arg));

    SEXP arg_tag = (TAG)(arg);

    if(arg_tag != R_NilValue)
    {
      SET_STRING_ELT(names, i, (PRINTNAME)(arg_tag));
    }

    arg = CDR(arg);
  }

  Rf_setAttrib(dots, R_NamesSymbol, names);

  UNPROTECT(3);

  return dots;
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

  if(!Rf_isNumeric(target_value))
  {
    Rf_errorcall(R_NilValue, "unable to differentiate object of type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(target_value)), cg_node_name(target));
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

  if(!Rf_isNumeric(node_value))
  {
    Rf_errorcall(R_NilValue, "unable to differentiate with respect to an object of type '%s' for node '%s'",
                 Rf_type2char(TYPEOF(node_value)), cg_node_name(target));
  }

  if(!Rf_isReal(node_value))
  {
    REPROTECT(node_value = Rf_coerceVector(node_value, REALSXP), node_index);

    cg_node_set_value(node, node_value);
  }

  int n = XLENGTH(node_value);

  SEXP grad = PROTECT(Rf_allocVector(REALSXP, n));

  double *pn = REAL(node_value);
  double *pg = REAL(grad);

  double eps = Rf_asReal(epsilon);

  for(int i = 0; i < n; i++)
  {
    pn[i] += eps;

    cg_graph_forward(graph, target);

    REPROTECT(target_value = cg_node_value(target), target_index);

    double t1 = REAL(target_value)[k - 1];

    pn[i] -= 2 * eps;

    cg_graph_forward(graph, target);

    REPROTECT(target_value = cg_node_value(target), target_index);

    double t2 = REAL(target_value)[k - 1];

    pg[i] = (t1 - t2) / (2 * eps);

    pn[i] += eps;
  }

  SHALLOW_DUPLICATE_ATTRIB(grad, node_value);

  cg_graph_forward(graph, target);

  UNPROTECT(3);

  return grad;
}
