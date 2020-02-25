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

#define MATH1(n0, i0, expr)\
  do{\
    for(; i0 < n0; i0++)\
      expr;\
  }while(0)

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

#define SWITCH1(x0, expr)\
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
        Rf_errorcall(R_NilValue, "type '%s' not supported", Rf_type2char(TYPEOF(x0)));\
    }\
  }while(0)

#define SWITCH2(x0, x1, expr) SWITCH1(x0, SWITCH1(x1, expr))
#define SWITCH3(x0, x1, x2, expr) SWITCH2(x0, x1, SWITCH1(x2, expr))

/*
 * PRIVATE FUNCTIONS
 */

static inline SEXP cg_allocate1(SEXPTYPE type, R_len_t n)
{
  return Rf_allocVector(type, n);
}

static inline SEXP cg_allocate2(SEXPTYPE type1, SEXPTYPE type2, R_len_t n)
{
  switch(type1)
  {
    case REALSXP :
    {
      switch(type2)
      {
        case REALSXP :
        case LGLSXP :
        case INTSXP :
          return cg_allocate1(REALSXP, n);
        default :
          Rf_errorcall(R_NilValue, "cannot allocate object of type '%s'", Rf_type2char(type2));
      }

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      switch(type2)
      {
        case REALSXP :
          return cg_allocate1(REALSXP, n);
        case LGLSXP :
        case INTSXP :
          return cg_allocate1(INTSXP, n);
        default :
          Rf_errorcall(R_NilValue, "cannot allocate object of type '%s'", Rf_type2char(type2));
      }

      break;
    }
    default :
      Rf_errorcall(R_NilValue, "cannot allocate object of type '%s'", Rf_type2char(type1));
  }
}

static inline int cg_conformable(SEXP x, SEXP y)
{
  SEXP dx = PROTECT(Rf_getAttrib(x, R_DimSymbol));
  SEXP dy = PROTECT(Rf_getAttrib(y, R_DimSymbol));

  if(dx == R_NilValue || dy == R_NilValue)
  {
    UNPROTECT(2);

    return TRUE;
  }

  R_len_t nx = XLENGTH(x);

  if(nx != XLENGTH(y))
  {
    UNPROTECT(2);

    return FALSE;
  }

  for(int i = 0; i < nx; i++)
  {
    if(INTEGER(dx)[i] != INTEGER(dy)[i])
    {
      UNPROTECT(2);

      return FALSE;
    }
  }

  UNPROTECT(2);

  return TRUE;
}

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

  if(!cg_conformable(x, y))
  {
    Rf_errorcall(R_NilValue, "argument 'x' and 'y' are non-conformable");
  }

  R_len_t nx = XLENGTH(x), ny = XLENGTH(y), nout = MAX2(nx, ny);

  if(!Rf_isReal(out) || XLENGTH(out) != nout)
  {
    PROTECT(out = cg_allocate2(TYPEOF(x), TYPEOF(y), nout));
  }
  else
  {
    PROTECT(out);
  }

  int ix = 0, iy = 0, iout = 0;

  SWITCH3(out, x, y, MATH3(nout, nx, ny, iout, ix, iy,
                          pout[iout] = px[ix] + py[iy]));

  if(nout == nx && ATTRIB(x) != R_NilValue)
  {
    SHALLOW_DUPLICATE_ATTRIB(out, x);
  }

  if(nout == ny && ATTRIB(y) != R_NilValue)
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
    PROTECT(out = cg_allocate1(REALSXP, n));
  }
  else
  {
    PROTECT(out);
  }

  int i = 0;

  double *pout = REAL(out);

  SWITCH1(x, MATH1(n, i, pout[i] = sin(px[i])));

  if(ATTRIB(x) != R_NilValue)
  {
    SHALLOW_DUPLICATE_ATTRIB(out, x);
  }

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

  int i = 0;

  double *pg = REAL(grad);
  double *po = REAL(out);

  SWITCH1(x, MATH1(n, i, po[i] += pg[i] * cos(px[i])));

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

  SWITCH1(x, for(int i = 0; i < n; i++)
             {
                po[i] = 1 / (1 + exp(-px[i]));
                po[i] = (po[i] < min) ? min : po[i];
                po[i] = (po[i] > max) ? max : po[i];
             }
  );

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
