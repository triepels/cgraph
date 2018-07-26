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
