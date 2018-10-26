/*
Copyright 2018 Ron Triepels

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

#include <inttypes.h>

SEXP bsum(SEXP x, SEXP n)
{
  if(!Rf_isNumeric(n))
  {
    Rf_errorcall(R_NilValue, "n must be a numerical scalar");
  }

  int x_n = Rf_asInteger(n);

  if(x_n < 0)
  {
    Rf_errorcall(R_NilValue, "invalid block size");
  }

  SEXP y = R_NilValue;

  switch(TYPEOF(x))
  {
    case LGLSXP :
    case INTSXP :
    {
      int* p_x;
      int* p_y;

      y = PROTECT(Rf_allocVector(TYPEOF(x), x_n));

      p_x = INTEGER(x);
      p_y = INTEGER(y);

      memset(p_y, 0, x_n * sizeof(int));

      int k = LENGTH(x), j = 0;

      for(int i = 0; i < k; i++)
      {
        p_y[j] += p_x[i];

        j = j < x_n - 1 ? j + 1 : 0;
      }

      break;
    }
    case REALSXP :
    {
      double* p_x;
      double* p_y;

      y = PROTECT(Rf_allocVector(TYPEOF(x), x_n));

      p_x = REAL(x);
      p_y = REAL(y);

      memset(p_y, 0, x_n * sizeof(double));

      int k = LENGTH(x), j = 0;

      for(int i = 0; i < k; i++)
      {
        p_y[j] += p_x[i];

        j = j < x_n - 1 ? j + 1 : 0;
      }

      break;
    }
    default :
    {
      Rf_errorcall(R_NilValue, "invalid object of type '%s' provided", Rf_type2char(TYPEOF(x)));
    }
  }

  UNPROTECT(1);

  return y;
}
