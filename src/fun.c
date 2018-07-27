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

#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>

SEXP address(SEXP graph)
{
  char address[32];

  sprintf(address, "0x%" PRIxPTR, (uintptr_t)graph);

  return(mkString(address));
}

SEXP bsum(SEXP x, SEXP n)
{
  int k = LENGTH(x), j = 0, nx;

  double * px, * py;

  nx = INTEGER(n)[0];

  SEXP y = PROTECT(allocVector(REALSXP, nx));

  x = coerceVector(x, REALSXP);

  px = REAL(x);
  py = REAL(y);

  memset(py, 0, nx * sizeof(double));

  for(int i = 0; i < k; i++)
  {
    py[j] += px[i];

    if(j < nx - 1)
    {
      j++;
    }
    else
    {
      j = 0;
    }
  }

  UNPROTECT(1);

  return y;
}
