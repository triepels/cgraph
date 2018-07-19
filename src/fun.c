#include <R.h>
#include <Rinternals.h>

SEXP bsum(SEXP x, SEXP n)
{
  int k = LENGTH(x), j = 0, n_val;

  double * x_val, * y_val;

  n_val = INTEGER(n)[0];

  if(n_val < 1 || n_val > k)
  {
    error("invalid block size n");
  }

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

SEXP array0(SEXP x, SEXP dim, SEXP dimnames)
{
  long int n = LENGTH(x), k = 1, l = 0;

  double * x_val, * y_val;

  for(int i = 0; i < LENGTH(dim); i++)
  {
    k *= INTEGER(dim)[i];
  }

  SEXP y = PROTECT(allocVector(REALSXP, k));

  x = coerceVector(x, REALSXP);

  x_val = REAL(x);
  y_val = REAL(y);

  memset(y_val, 0, k * sizeof(double));

  (k <= n) ? (l = k) : (l = n);

  for(int i = 0; i < l; i++)
  {
    y_val[i] = x_val[i];
  }

  setAttrib(y, R_DimSymbol, dim);

  if(!isNull(dimnames))
  {
    setAttrib(y, R_DimNamesSymbol, dimnames);
  }

  UNPROTECT(1);

  return y;
}
