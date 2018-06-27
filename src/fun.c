#include <R.h>
#include <Rinternals.h>

SEXP bsum(SEXP x, SEXP n)
{
  int j = 0, k, * n_val;

  double * x_val, * y_val;

  n_val = INTEGER(n);

  k = LENGTH(x);

  if(n_val[0] < 1 || n_val[0] > k)
  {
    error("invalid block size n");
  }

  SEXP y = PROTECT(allocVector(REALSXP, n_val[0]));

  x = coerceVector(x, REALSXP);

  x_val = REAL(x);
  y_val = REAL(y);

  memset(y_val, 0, n_val[0] * sizeof(double));

  for(int i = 0; i < k; i++)
  {
    y_val[j] += x_val[i];

    if(j < n_val[0] - 1)
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
