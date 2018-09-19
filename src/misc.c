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

SEXP sigmoid(SEXP x, SEXP eps)
{
  double * py;

  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "x must be a numerical vector or array");
  }

  if(!Rf_isNumber(eps))
  {
    Rf_errorcall(R_NilValue, "eps must be a numeric scalar");
  }

  PROTECT_INDEX ipy;

  SEXP y = R_NilValue;

  PROTECT_WITH_INDEX(y = Rf_duplicate(x), &ipy);

  if(!Rf_isReal(y))
  {
    REPROTECT(y = Rf_coerceVector(y, REALSXP), ipy);
  }

  py = REAL(y);

  const double min = Rf_asReal(eps), max = 1 - min;

  for(int i = 0; i < LENGTH(x); i++)
  {
    py[i] = 1 / (1 + exp(-py[i]));

    py[i] = py[i] < min ? min : py[i];

    py[i] = py[i] > max ? max : py[i];
  }

  UNPROTECT(1);

  return y;
}
