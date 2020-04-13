/*
Copyright 2020 Ron Triepels

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
#include <Rmath.h>
#include <Rinternals.h>

#include "node.h"
#include "initializer.h"

/*
 * PRIVATE FUNCTIONS
 */

// Note: Rf_allocArray does not handle non-integer dimensions.
static SEXP cg_array(SEXP dim)
{
  if(!Rf_isNumeric(dim))
  {
    Rf_errorcall(R_NilValue, "argument 'dim' must be a numeric vector");
  }

  R_len_t n = 1, m = XLENGTH(dim);

  SEXP out_dim = PROTECT(Rf_allocVector(INTSXP, m));

  for(int i = 0; i < m; i++)
  {
    switch(TYPEOF(dim))
    {
      case REALSXP :
      {
        double x = REAL(dim)[i];

        if(x < 0)
        {
          Rf_errorcall(R_NilValue, "negative dimensions are not allowed");
        }

        if(x != (int)x)
        {
          Rf_errorcall(R_NilValue, "all dimensions must be whole numbers");
        }

        INTEGER(out_dim)[i] = x;

        n *= x;

        break;
      }
      case INTSXP :
      case LGLSXP :
      {
        int x = INTEGER(dim)[i];

        if(x < 0)
        {
          Rf_errorcall(R_NilValue, "negative dimensions are not allowed");
        }

        INTEGER(out_dim)[i] = x;

        n *= x;

        break;
      }
    }
  }

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  Rf_setAttrib(out, R_DimSymbol, out_dim);

  UNPROTECT(2);

  return out;
}

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_init_zeros(SEXP dim, SEXP name)
{
  SEXP value = PROTECT(cg_array(dim));

  memset(REAL(value), 0, XLENGTH(value) * sizeof(double));

  UNPROTECT(1);

  return cg_parameter(value, name);
}

SEXP cg_init_ones(SEXP dim, SEXP name)
{
  SEXP value = PROTECT(cg_array(dim));

  R_len_t n = XLENGTH(value);

  double *pv = REAL(value);

  for(int i = 0; i < n; i++)
  {
    pv[i] = 1;
  }

  UNPROTECT(1);

  return cg_parameter(value, name);
}

SEXP cg_init_uniform(SEXP dim, SEXP min, SEXP max, SEXP name)
{
  if(!Rf_isNumeric(min) || XLENGTH(min) != 1)
  {
    Rf_errorcall(R_NilValue, "argument 'min' must be a numeric scalar");
  }

  if(!Rf_isNumeric(max) || XLENGTH(max) != 1)
  {
    Rf_errorcall(R_NilValue, "argument 'max' must be a numeric scalar");
  }

  SEXP value = PROTECT(cg_array(dim));

  R_len_t n = XLENGTH(value);

  double *pv = REAL(value);

  double a = Rf_asReal(min), b = Rf_asReal(max);

  for(int i = 0; i < n; i++)
  {
    pv[i] = Rf_runif(a, b);
  }

  UNPROTECT(1);

  return cg_parameter(value, name);
}

SEXP cg_init_gaussian(SEXP dim, SEXP mean, SEXP sd, SEXP name)
{
  if(!Rf_isNumeric(mean) || XLENGTH(mean) != 1)
  {
    Rf_errorcall(R_NilValue, "argument 'mean' must be a numeric scalar");
  }

  if(!Rf_isNumeric(sd) || XLENGTH(sd) != 1)
  {
    Rf_errorcall(R_NilValue, "argument 'sd' must be a numeric scalar");
  }

  SEXP value = PROTECT(cg_array(dim));

  R_len_t n = XLENGTH(value);

  double *pv = REAL(value);

  double m = Rf_asReal(mean), s = Rf_asReal(sd);

  for(int i = 0; i < n; i++)
  {
    pv[i] = Rf_rnorm(m, s);
  }

  UNPROTECT(1);

  return cg_parameter(value, name);
}

SEXP cg_init_xavier_uniform(SEXP dim, SEXP name)
{
  if(XLENGTH(dim) < 2)
  {
    Rf_errorcall(R_NilValue, "argument 'dim' must have at least two dimensions");
  }

  SEXP value = PROTECT(cg_array(dim));

  SEXP value_dim = PROTECT(Rf_getAttrib(value, R_DimSymbol));

  R_len_t n = XLENGTH(value);

  double *pv = REAL(value);

  double b = sqrt(6 / (double)(INTEGER(value_dim)[0] + INTEGER(value_dim)[1]));

  for(int i = 0; i < n; i++)
  {
    pv[i] = Rf_runif(-b, b);
  }

  UNPROTECT(2);

  return cg_parameter(value, name);
}

SEXP cg_init_xavier_gaussian(SEXP dim, SEXP name)
{
  if(XLENGTH(dim) < 2)
  {
    Rf_errorcall(R_NilValue, "argument 'dim' must have at least two dimensions");
  }

  SEXP value = PROTECT(cg_array(dim));

  SEXP value_dim = PROTECT(Rf_getAttrib(value, R_DimSymbol));

  R_len_t n = XLENGTH(value);

  double *pv = REAL(value);

  double s = sqrt(2 / (double)(INTEGER(value_dim)[0] + INTEGER(value_dim)[1]));

  for(int i = 0; i < n; i++)
  {
    pv[i] = Rf_rnorm(0, s);
  }

  UNPROTECT(2);

  return cg_parameter(value, name);
}
