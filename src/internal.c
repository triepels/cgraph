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

SEXP address(SEXP x)
{
  char address[32];

  sprintf(address, "0x%" PRIxPTR, (uintptr_t)x);

  return(Rf_mkString(address));
}

SEXP bsum(SEXP x, SEXP n)
{
  double * px, * py;

  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "x must be a numerical vector or array");
  }

  if(!Rf_isNumeric(n))
  {
    Rf_errorcall(R_NilValue, "n must be a non-negative numerical scalar");
  }

  if(Rf_asInteger(n) < 0)
  {
    Rf_errorcall(R_NilValue, "invalid block size");
  }

  int nx = Rf_asInteger(n), k = LENGTH(x), j = 0;

  SEXP y = PROTECT(Rf_allocVector(REALSXP, nx));

  x = PROTECT(Rf_coerceVector(x, REALSXP));

  px = REAL(x);
  py = REAL(y);

  memset(py, 0, nx * sizeof(double));

  for(int i = 0; i < k; i++)
  {
    py[j] += px[i];

    j = j < nx - 1 ? j + 1 : 0;
  }

  UNPROTECT(2);

  return y;
}
