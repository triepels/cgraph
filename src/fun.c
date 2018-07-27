/*
Copyright (C) 2018 Ron Triepels

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
