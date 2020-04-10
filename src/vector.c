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

#include "vector.h"

/*
 * PUBLIC FUNCTIONS
 */

SEXP copy(SEXP x, SEXP y, SEXP offset)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(y))
  {
    Rf_errorcall(R_NilValue, "argument 'y' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(offset) || XLENGTH(offset) != 1)
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical scalar");
  }

  R_len_t nx = XLENGTH(x);
  R_len_t ny = XLENGTH(y);

  if(nx > ny)
  {
    Rf_errorcall(R_NilValue, "cannot copy %d elements to a vector of length %d", nx, ny);
  }

  int k = Rf_asInteger(offset);

  if(k < 0 || k > ny - nx)
  {
    Rf_errorcall(R_NilValue, "offset out of bounds");
  }

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      double *px = REAL(x);

      switch(TYPEOF(y))
      {
        case REALSXP :
        {
          double *py = REAL(y);

          memcpy(py + k, px, nx * sizeof(double));

          break;
        }
        case INTSXP :
        case LGLSXP :
        {
          int *py = INTEGER(y);

          for(int i = 0; i < nx; i++)
          {
            py[k + i] = px[i];
          }

          break;
        }
      }

      break;
    }
    case INTSXP :
    case LGLSXP :
    {
      int *px = INTEGER(x);

      switch(TYPEOF(y))
      {
        case REALSXP :
        {
          double *py = REAL(y);

          for(int i = 0; i < nx; i++)
          {
            py[k + i] = px[i];
          }

          break;
        }
        case INTSXP :
        case LGLSXP :
        {
          int *py = INTEGER(y);

          memcpy(py + k, px, nx * sizeof(int));

          break;
        }
      }

      break;
    }
  }

  return y;
}

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
