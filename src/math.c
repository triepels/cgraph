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

#include "math.h"

/*
 * PUBLIC METHODS
 */

SEXP sigmoid(SEXP x)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  R_len_t n = XLENGTH(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  double *po = REAL(out);

  const double min = DBL_EPSILON, max = 1 - DBL_EPSILON;

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      double *px = REAL(x);

      for(int i = 0; i < n; i++)
      {
        po[i] = 1 / (1 + exp(-px[i]));

        po[i] = (po[i] < min) ? min : po[i];

        po[i] = (po[i] > max) ? max : po[i];
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *px = INTEGER(x);

      for(int i = 0; i < n; i++)
      {
        po[i] = 1 / (1 + exp(-px[i]));

        po[i] = (po[i] < min) ? min : po[i];

        po[i] = (po[i] > max) ? max : po[i];
      }

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(out, x);

  UNPROTECT(1);

  return out;
}

SEXP sigmoid_grad(SEXP x, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  if(!Rf_isReal(val))
  {
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  if(XLENGTH(x) != XLENGTH(val) || XLENGTH(val) != XLENGTH(grad))
  {
    Rf_errorcall(R_NilValue, "the lengths of argument 'x' (%d), 'val' (%d), and 'grad' (%d) are incompatible",
                 XLENGTH(x), XLENGTH(val), XLENGTH(grad));
  }

  R_len_t n = XLENGTH(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  double *pv = REAL(val);
  double *pg = REAL(grad);
  double *po = REAL(out);

  for(int i = 0; i < n; i++)
  {
    po[i] = pg[i] * pv[i] * (1 - pv[i]);
  }

  SHALLOW_DUPLICATE_ATTRIB(out, x);

  UNPROTECT(1);

  return out;
}
