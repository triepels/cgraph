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

#include "subset.h"

/*
 * PUBLIC FUNCTIONS
 */

SEXP slice(SEXP x, SEXP index)
{
  if(!Rf_isArray(x) || !Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical array");
  }

  SEXP dim = PROTECT(Rf_getAttrib(x, R_DimSymbol));

  if(XLENGTH(dim) < 3)
  {
    Rf_errorcall(R_NilValue, "argument 'x' must have at least three dimensions");
  }

  int k = Rf_asInteger(index);

  int a = INTEGER(dim)[0], b = INTEGER(dim)[1], c = a * b;

  if(k < 1 || c * k > XLENGTH(x))
  {
    Rf_errorcall(R_NilValue, "argument 'index' is out of bounds");
  }

  SEXP out;

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      PROTECT(out = Rf_allocMatrix(REALSXP, a, b));

      memcpy(REAL(out), REAL(x) + c * (k - 1), c * sizeof(double));

      break;
    }
    case INTSXP :
    {
      PROTECT(out = Rf_allocMatrix(INTSXP, a, b));

      memcpy(INTEGER(out), INTEGER(x) + c * (k - 1), c * sizeof(int));

      break;
    }
    case LGLSXP :
    {
      PROTECT(out = Rf_allocMatrix(INTSXP, a, b));

      memcpy(INTEGER(out), INTEGER(x) + c * (k - 1), c * sizeof(int));

      break;
    }
    default :
    {
      Rf_errorcall(R_NilValue, "cannot slice object of type '%s'",
                   Rf_type2char(TYPEOF(x)));
    }
  }

  UNPROTECT(2);

  return out;
}

SEXP slice_assign(SEXP x, SEXP index, SEXP y)
{
  if(!Rf_isArray(x) || !Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical array");
  }

  if(!Rf_isMatrix(y) || !Rf_isNumeric(y))
  {
    Rf_errorcall(R_NilValue, "argument 'y' must be a numerical matrix");
  }

  SEXP dim_x = PROTECT(Rf_getAttrib(x, R_DimSymbol));

  if(XLENGTH(dim_x) < 3)
  {
    Rf_errorcall(R_NilValue, "argument 'x' must have at least three dimensions");
  }

  int k = Rf_asInteger(index);

  int a = INTEGER(dim_x)[0], b = INTEGER(dim_x)[1], c = a * b;

  if(k < 1 || c * k > XLENGTH(x))
  {
    Rf_errorcall(R_NilValue, "argument 'index' is out of bounds");
  }

  SEXP dim_y = PROTECT(Rf_getAttrib(y, R_DimSymbol));

  if(INTEGER(dim_y)[0] != a || INTEGER(dim_y)[1] != b)
  {
    Rf_errorcall(R_NilValue, "argument 'y' has incompatible dimensions");
  }

  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      double *po = REAL(x);

      po += c * (k - 1);

      switch(TYPEOF(y))
      {
        case REALSXP :
        {
          double *py = REAL(y);

          memcpy(po, py, c * sizeof(double));

          break;
        }
        case INTSXP :
        case LGLSXP :
        {
          int *py = INTEGER(y);

          for(int i = 0; i < c; i++)
          {
            po[i] = py[i];
          }

          break;
        }
      }

      break;
    }
    case INTSXP :
    case LGLSXP :
    {
      int *po = INTEGER(x);

      po += c * (k - 1);

      switch(TYPEOF(y))
      {
        case REALSXP :
        {
          double *py = REAL(y);

          for(int i = 0; i < c; i++)
          {
            po[i] = py[i];
          }

          break;
        }
        case INTSXP :
        case LGLSXP :
        {
          int *py = INTEGER(y);

          memcpy(po, py, c * sizeof(int));

          break;
        }
      }
    }

  }

  UNPROTECT(2);

  return x;
}
