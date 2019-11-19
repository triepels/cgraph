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

  SEXP y = PROTECT(Rf_allocVector(REALSXP, m));

  double *b = REAL(y);

  memset(b, 0, m * sizeof(double));

  R_len_t n = XLENGTH(x);

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      double *a = REAL(x);

      for(int i = 0, j = 0; i < n; i++,
          j = (++j == m) ? 0 : j)
      {
        b[j] += a[i];
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *a = INTEGER(x);

      for(int i = 0, j = 0; i < n; i++,
          j = (++j == m) ? 0 : j)
      {
        b[j] += a[i];
      }

      break;
    }
  }

  UNPROTECT(1);

  return y;
}

SEXP approx_gradients(SEXP graph, SEXP target, SEXP values, SEXP gradients, SEXP index, SEXP epsilon)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  if(!cg_is(target, "cg_node"))
  {
    Rf_errorcall(R_NilValue, "argument 'target' must be a cg_node object");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "argument 'values' must be an environment");
  }

  if(!Rf_isEnvironment(gradients))
  {
    Rf_errorcall(R_NilValue, "argument 'gradients' must be an environment");
  }

  if(!Rf_isNumeric(index))
  {
    Rf_errorcall(R_NilValue, "argument 'index' must be a numeric scalar");
  }

  if(!Rf_isNumeric(epsilon))
  {
    Rf_errorcall(R_NilValue, "argument 'epsilon' must be a numeric scalar");
  }

  int k = Rf_asInteger(index);

  double eps = Rf_asReal(epsilon);

  if(eps < 0)
  {
    Rf_errorcall(R_NilValue, "argument 'epsilon' must be non-negative");
  }

  int target_index;

  SEXP target_value = R_NilValue;

  SEXP target_symbol = cg_node_symbol(target);

  PROTECT_WITH_INDEX(target_value = Rf_findVarInFrame(values, target_symbol), &target_index);

  if(target_value == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "cannot find value of node '%s'", cg_node_name(target));
  }

  if(!Rf_isReal(target_value))
  {
    REPROTECT(target_value = Rf_coerceVector(target_value, REALSXP), target_index);

    cg_node_set_value(target, target_value);
  }

  if(k < 1 || k > XLENGTH(target_value))
  {
    Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at index %d", cg_node_name(target), k);
  }

  SEXP nodes = PROTECT(cg_graph_reverse_dfs(graph, target));

  R_len_t n = XLENGTH(nodes);

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(cg_node_type(node) == CGPRM)
    {
      int node_index;

      SEXP node_value = R_NilValue;

      SEXP node_symbol = cg_node_symbol(node);

      PROTECT_WITH_INDEX(node_value = Rf_findVarInFrame(values, node_symbol), &node_index);

      if(node_value == R_UnboundValue)
      {
        Rf_errorcall(R_NilValue, "cannot find value of node '%s'", cg_node_name(node));
      }

      if(!Rf_isReal(node_value))
      {
        REPROTECT(node_value = Rf_coerceVector(node_value, REALSXP), node_index);

        cg_node_set_value(node, node_value);
      }

      R_len_t m = XLENGTH(node_value);

      SEXP grad = PROTECT(Rf_allocVector(REALSXP, m));

      double *x = REAL(node_value);
      double *y = REAL(grad);

      for(int j = 0; j < m; j++)
      {
        x[j] += eps;

        cg_graph_run(graph, target, values);

        REPROTECT(target_value = Rf_findVarInFrame(values, target_symbol), target_index);

        double y1 = REAL(target_value)[k - 1];

        x[j] -= 2 * eps;

        cg_graph_run(graph, target, values);

        REPROTECT(target_value = Rf_findVarInFrame(values, target_symbol), target_index);

        double y2 = REAL(target_value)[k - 1];

        x[j] += eps;

        y[j] = (y1 - y2) / (2 * eps);
      }

      SHALLOW_DUPLICATE_ATTRIB(grad, node_value);

      Rf_defineVar(node_symbol, grad, gradients);

      UNPROTECT(2);
    }
  }

  cg_graph_run(graph, target, values);

  UNPROTECT(2);

  return gradients;
}
