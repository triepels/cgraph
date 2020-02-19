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
#include <Rinternals.h>

#include "vector.h"

/*
 * MACROS
 */

#define INC(i, n) (++i < n) ? i : 0

#define MAX2(x0, x1) (x0 >= x1 ? x0 : x1)
#define MAX3(x0, x1, x2) MAX2(MAX2(x0, x1), x2)

#define MATH2(n0, n1, i0, i1, expr)\
  do{\
    if(n0 == n1)\
      for(; i0 < n0; i0++, i1++)\
        expr;\
    else if(n0 == 1)\
      for(; i1 < n1; i1++)\
        expr;\
    else\
    {\
      if(n1 == 1)\
        for(; i0 < n0; i0++)\
          expr;\
      else\
        for(int i_ = 0; i_ < MAX2(n0, n1); i_++,\
            i0 = INC(i0, n0), i1 = INC(i1, n1))\
          expr;\
    }\
  }while(0)

#define MATH3(n0, n1, n2, i0, i1, i2, expr)\
  do{\
    if(n0 == n1 && n0 == n2)\
      for(; i0 < n0; i0++, i1++, i2++)\
        expr;\
    else if(n0 == 1)\
    {\
      if(n1 == 1)\
      {\
        for(; i2 < n2; i2++)\
          expr;\
      }\
      else\
      {\
        if(n2 == 1)\
          for(; i1 < n1; i1++)\
            expr;\
        else\
          for(int i_ = 0; i_ < MAX2(n1, n2); i_++,\
              i1 = INC(i1, n1), i2 = INC(i2, n2))\
            expr;\
      }\
    }\
    else\
    {\
      if(n1 == 1)\
      {\
        if(n2 == 1)\
          for(; i0 < n0; i0++)\
            expr;\
        else\
          for(int i_ = 0; i_ < MAX2(n0, n2); i_++,\
              i0 = INC(i0, n0), i2 = INC(i2, n2))\
            expr;\
      }\
      else\
      {\
        if(n2 == 1)\
          for(int i_ = 0; i_ < MAX2(n0, n1); i_++,\
              i0 = INC(i0, n0), i1 = INC(i1, n1))\
            expr;\
        else\
          for(int i_ = 0; i_ < MAX3(n0, n1, n2); i_++,\
              i0 = INC(i0, n0), i1 = INC(i1, n1), i2 = INC(i2, n2))\
            expr;\
      }\
    }\
  }while(0)

#define NUMERIC_SWITCH1(x0, expr)\
  do{\
    switch(TYPEOF(x0))\
    {\
      case REALSXP :\
      {\
        double *p##x0 = REAL(x0);\
        expr;\
        break;\
      }\
      case LGLSXP :\
      case INTSXP :\
      {\
        int *p##x0 = INTEGER(x0);\
        expr;\
        break;\
      }\
      default :\
      { /* Should not happen but for safety */\
        Rf_errorcall(R_NilValue, "type '%s' not supported", Rf_type2char(TYPEOF(x0)));\
      }\
    }\
  }while(0)

#define NUMERIC_SWITCH2(x0, x1, expr) NUMERIC_SWITCH1(x0, NUMERIC_SWITCH1(x1, expr))

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_add_def(SEXP x, SEXP y, SEXP out)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(y))
  {
    Rf_errorcall(R_NilValue, "argument 'y' must be a numerical vector or array");
  }

  R_len_t nx = XLENGTH(x), ny = XLENGTH(y), no = MAX2(nx, ny);

  if(!Rf_isReal(out) || XLENGTH(out) != no)
  {
    PROTECT(out = Rf_allocVector(REALSXP, no));
  }
  else
  {
    PROTECT(out);
  }

  int ix = 0, iy = 0, io = 0;

  double *po = REAL(out);

  NUMERIC_SWITCH2(x, y,
    MATH3(nx, ny, no, ix, iy, io,
          po[io] = px[ix] + py[iy]));

  if(no == nx)
  {
    SHALLOW_DUPLICATE_ATTRIB(out, x);
  }

  if(no == ny)
  {
    SHALLOW_DUPLICATE_ATTRIB(out, y);
  }

  UNPROTECT(1);

  return out;
}

SEXP cg_sin_def(SEXP x, SEXP out)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  R_len_t n = XLENGTH(x);

  if(!Rf_isReal(out) || XLENGTH(out) != n)
  {
    PROTECT(out = Rf_allocVector(REALSXP, n));
  }
  else
  {
    PROTECT(out);
  }

  double *po = REAL(x);

  NUMERIC_SWITCH1(x,
    for(int i = 0; i < n; i++)
    {
      po[i] = sin(px[i]);
    });

  SHALLOW_DUPLICATE_ATTRIB(out, x);

  UNPROTECT(1);

  return out;
}

SEXP cg_sin_grad(SEXP x, SEXP grad, SEXP out)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  if(!Rf_isReal(out))
  {
    Rf_errorcall(R_NilValue, "argument 'out' must be a real vector or array");
  }

  R_len_t n = XLENGTH(x);

  if(XLENGTH(grad) != n || XLENGTH(out) != n)
  {
    Rf_errorcall(R_NilValue, "argument 'x', 'grad', and 'out' have incompatible lengths");
  }

  double *pg = REAL(grad);
  double *po = REAL(out);

  NUMERIC_SWITCH1(x,
    for(int i = 0; i < n; i++)
    {
      po[i] += pg[i] * cos(px[i]);
    });

  return out;
}

SEXP cg_sigmoid_def(SEXP x, SEXP out)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  R_len_t n = XLENGTH(x);

  if(!Rf_isReal(out) || XLENGTH(out) != n)
  {
    PROTECT(out = Rf_allocVector(REALSXP, n));
  }
  else
  {
    PROTECT(out);
  }

  double *po = REAL(out);

  const double min = DBL_EPSILON, max = 1 - DBL_EPSILON;

  NUMERIC_SWITCH1(x,
    for(int i = 0; i < n; i++)
    {
      po[i] = 1 / (1 + exp(-px[i]));
      po[i] = (po[i] < min) ? min : po[i];
      po[i] = (po[i] > max) ? max : po[i];
    });

  /*
  switch(TYPEOF(x))
  {
    case REALSXP :
    {
      double *px = REAL(x);

      for(int i = 0; i < n; i++)
      {
        po[i] = 1 / (1 + exp(-px[i]));

        po[i] = (po[i] < min) ? min : po[i];

        po[i] = (po[i] > max) ? max : po[i];
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *px = INTEGER(x);

      for(int i = 0; i < n; i++)
      {
        po[i] = 1 / (1 + exp(-px[i]));

        po[i] = (po[i] < min) ? min : po[i];

        po[i] = (po[i] > max) ? max : po[i];
      }

      break;
    }
  }
  */

  SHALLOW_DUPLICATE_ATTRIB(out, x);

  UNPROTECT(1);

  return out;
}

SEXP cg_sigmoid_grad(SEXP value, SEXP grad, SEXP out)
{
  if(!Rf_isReal(value))
  {
    Rf_errorcall(R_NilValue, "argument 'value' must be a real vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  if(!Rf_isReal(out))
  {
    Rf_errorcall(R_NilValue, "argument 'out' must be a real vector or array");
  }

  R_len_t n = XLENGTH(value);

  if(XLENGTH(grad) != n || XLENGTH(out) != n)
  {
    Rf_errorcall(R_NilValue, "argument 'value', 'grad', and 'out' have incompatible lengths");
  }

  double *pv = REAL(value);
  double *pg = REAL(grad);
  double *po = REAL(out);

  for(int i = 0; i < n; i++)
  {
    po[i] += pg[i] * pv[i] * (1 - pv[i]);
  }

  return out;
}
