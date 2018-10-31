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

SEXP sigmoid(SEXP x)
{
  int i_y;

  SEXP y = R_NilValue;

  PROTECT_WITH_INDEX(y = Rf_duplicate(x), &i_y);

  switch(TYPEOF(y))
  {
    case INTSXP :
    {
      REPROTECT(y = Rf_coerceVector(y, REALSXP), i_y);

      break;
    }
    case REALSXP :
    {
      break;
    }
    default :
    {
      Rf_errorcall(R_NilValue, "invalid object of type '%s' provided", Rf_type2char(TYPEOF(y)));
    }
  }

  int n = LENGTH(y);

  double* p_y = REAL(y);

  const double min = DBL_EPSILON, max = 1 - DBL_EPSILON;

  for(int i = 0; i < n; i++)
  {
    p_y[i] = 1 / (1 + exp(-p_y[i]));

    p_y[i] = p_y[i] < min ? min : p_y[i];

    p_y[i] = p_y[i] > max ? max : p_y[i];
  }

  UNPROTECT(1);

  return y;
}
