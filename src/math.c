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
  if(!(Rf_isLogical(x) || Rf_isNumeric(x)))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  R_len_t n = Rf_xlength(x);

  SEXP y = PROTECT(Rf_allocVector(REALSXP, n));

  double *b = REAL(y);

  const double min = DBL_EPSILON, max = 1 - DBL_EPSILON;

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      const double *a = REAL_RO(x);

      for(int i = 0; i < n; i++)
      {
        b[i] = 1 / (1 + exp(-a[i]));

        b[i] = (b[i] < min) ? min : b[i];

        b[i] = (b[i] > max) ? max : b[i];
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      const int *a = INTEGER_RO(x);

      for(int i = 0; i < n; i++)
      {
        b[i] = 1 / (1 + exp(-a[i]));

        b[i] = (b[i] < min) ? min : b[i];

        b[i] = (b[i] > max) ? max : b[i];
      }

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(y, x);

  UNPROTECT(1);

  return y;
}
