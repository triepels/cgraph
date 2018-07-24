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
  int k = LENGTH(x), j = 0, n_val;

  double * x_val, * y_val;

  n_val = INTEGER(n)[0];

  SEXP y = PROTECT(allocVector(REALSXP, n_val));

  x = coerceVector(x, REALSXP);

  x_val = REAL(x);
  y_val = REAL(y);

  memset(y_val, 0, n_val * sizeof(double));

  for(int i = 0; i < k; i++)
  {
    y_val[j] += x_val[i];

    if(j < n_val - 1)
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
