/*
Copyright 2019 Ron Triepels

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

#include "math.h"

/*
 * PUBLIC METHODS
 */

#define INC(i, n) (++i < n) ? i : 0

#define MAX2(x0, x1) (x0 >= x1 ? x0 : x1)
#define MAX3(x0, x1, x2) MAX2(MAX2(x0, x1), x2)
#define MAX4(x0, x1, x2, x3) MAX2(MAX2(MAX2(x0, x1), x2), x3)

#define MATH2(n0, n1, i0, i1, expr)\
  do{\
    if(n0 == n1)\
      for(; i0 < n0; i0++, i1++)\
        expr;\
    else\
    {\
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
    else\
    {\
      if(n0 == 1)\
        for(int i_ = 0; i_ < MAX3(n0, n1, n2); i_++,\
            i1 = INC(i1, n1), i2 = INC(i2, n2))\
          expr;\
      else if(n1 == 1)\
        for(int i_ = 0; i_ < MAX3(n0, n1, n2); i_++,\
            i0 = INC(i0, n0), i2 = INC(i2, n2))\
          expr;\
      else if(n2 == 1)\
        for(int i_ = 0; i_ < MAX3(n0, n1, n2); i_++,\
            i0 = INC(i0, n0), i1 = INC(i1, n1))\
          expr;\
      else\
        for(int i_ = 0; i_ < MAX3(n0, n1, n2); i_++,\
            i0 = INC(i0, n0), i1 = INC(i1, n1), i2 = INC(i2, n2))\
          expr;\
    }\
  }while(0)

#define MATH4(n0, n1, n2, n3, i0, i1, i2, i3, expr)\
  do{\
    if(n0 == n1 && n0 == n2 && n0 == n3)\
      for(; i0 < n0; i0++, i1++, i2++, i3++)\
        expr;\
    else\
    {\
      if(n0 == 1)\
        for(int i_ = 0; i_ < MAX4(n0, n1, n2, n3); i_++,\
            i1 = INC(i1, n1), i2 = INC(i2, n2), i3 = INC(i3, n3))\
          expr;\
      else if(n1 == 1)\
        for(int i_ = 0; i_ < MAX4(n0, n1, n2, n3); i_++,\
            i0 = INC(i0, n0), i2 = INC(i2, n2), i3 = INC(i3, n3))\
          expr;\
      else if(n2 == 1)\
        for(int i_ = 0; i_ < MAX4(n0, n1, n2, n3); i_++,\
            i0 = INC(i0, n0), i1 = INC(i1, n1), i3 = INC(i3, n3))\
          expr;\
      else if(n3 == 1)\
        for(int i_ = 0; i_ < MAX4(n0, n1, n2, n3); i_++,\
            i0 = INC(i0, n0), i1 = INC(i1, n1), i2 = INC(i2, n2))\
          expr;\
      else\
        for(int i_ = 0; i_ < MAX4(n0, n1, n2, n3); i_++,\
            i0 = INC(i0, n0), i1 = INC(i1, n1), i2 = INC(i2, n2), i3 = INC(i3, n3))\
          expr;\
    }\
  }while(0)

SEXP cg_math_pos(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  return Rf_duplicate(a1);
}

SEXP cg_math_pos_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  SEXP a0 = PROTECT(Rf_duplicate(grad));

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_neg(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(TYPEOF(a1), n));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = -p1[i];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p0 = INTEGER(a0);
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = -p1[i];

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_neg_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1' and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  for(int i = 0; i < n; i++)
    p0[i] = -pg[i];

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_add(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  SEXP a0;

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n0 = MAX2(n1, n2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      PROTECT(a0 = Rf_allocVector(REALSXP, n0));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          PROTECT(a0 = Rf_allocVector(REALSXP, n0));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          PROTECT(a0 = Rf_allocVector(INTSXP, n0));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  else if(n0 == n2)
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_add_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n1));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n1 * sizeof(double));

  int i1 = 0, ig = 0;

  MATH2(n1, ng, i1, ig,
        p0[i1] += pg[ig]);

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_sub(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  SEXP a0;

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n0 = MAX2(n1, n2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      PROTECT(a0 = Rf_allocVector(REALSXP, n0));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          PROTECT(a0 = Rf_allocVector(REALSXP, n0));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          PROTECT(a0 = Rf_allocVector(INTSXP, n0));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  else if(n0 == n2)
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_sub_grad(SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n2));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n2 * sizeof(double));

  int i2 = 0, ig = 0;

  MATH2(n2, ng, i2, ig,
        p0[i2] += -pg[ig]);

  SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_mul(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  SEXP a0;

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n0 = MAX2(n1, n2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      PROTECT(a0 = Rf_allocVector(REALSXP, n0));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          PROTECT(a0 = Rf_allocVector(REALSXP, n0));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          PROTECT(a0 = Rf_allocVector(INTSXP, n0));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  else if(n0 == n2)
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_mul_grad1(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n1));

  int i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n1 * sizeof(double));

  switch(TYPEOF(a2))
  {
    case REALSXP :
    {
      double *p2 = REAL(a2);

      MATH3(n1, n2, ng, i1, i2, ig,
            p0[i1] += pg[ig] * p2[i2]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p2 = INTEGER(a2);

      MATH3(n1, n2, ng, i1, i2, ig,
            p0[i1] += pg[ig] * p2[i2]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_mul_grad2(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n2));

  int i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n2 * sizeof(double));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      MATH3(n1, n2, ng, i1, i2, ig,
            p0[i2] += pg[ig] * p1[i1]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      MATH3(n1, n2, ng, i1, i2, ig,
            p0[i2] += pg[ig] * p1[i1]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_div(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n0 = MAX2(n1, n2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n0));

  int i0 = 0, i1 = 0, i2 = 0;

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  else if(n0 == n2)
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_div_grad1(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n1));

  int i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n1 * sizeof(double));

  switch(TYPEOF(a2))
  {
    case REALSXP :
    {
      double *p2 = REAL(a2);

      MATH3(n1, n2, ng, i1, i2, ig,
            p0[i1] += pg[ig] / p2[i2]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p2 = INTEGER(a2);

      MATH3(n1, n2, ng, i1, i2, ig,
            p0[i1] += pg[ig] / p2[i2]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_div_grad2(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n2));

  int i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n2 * sizeof(double));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

          break;
        }
      }
      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_pow(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  SEXP a0;

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n0 = MAX2(n1, n2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      PROTECT(a0 = Rf_allocVector(REALSXP, n0));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          PROTECT(a0 = Rf_allocVector(REALSXP, n0));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          PROTECT(a0 = Rf_allocVector(INTSXP, n0));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else if(n0 == n2)
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_pow_grad1(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n1));

  int i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n1 * sizeof(double));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i1] += pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i1] += pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i1] += pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i1] += pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

          break;
        }
      }
      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_pow_grad2(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), ng = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n2));

  int i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n2 * sizeof(double));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n1, n2, ng, i1, i2, ig,
                p0[i2] += pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

          break;
        }
      }
      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_square(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  SEXP a0;

  R_len_t n = XLENGTH(a1);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = p1[i] * p1[i];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      a0 = PROTECT(Rf_allocVector(INTSXP, n));

      int *p0 = INTEGER(a0);
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = p1[i] * p1[i];

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_square_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1' and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = 2 * pg[i] * p1[i];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = 2 * pg[i] * p1[i];

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_sqrt(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = sqrt(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = sqrt(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_sqrt_grad(SEXP a1, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(val))
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(val) != n || XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1', 'val', and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pv = REAL(val);
  double *pg = REAL(grad);

  for(int i = 0; i < n; i++)
    p0[i] = pg[i] * 1 / (2 * pv[i]);

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_cbrt(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = cbrt(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = cbrt(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_cbrt_grad(SEXP a1, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(val))
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(val) != n || XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1', 'val', and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pv = REAL(val);
  double *pg = REAL(grad);

  for(int i = 0; i < n; i++)
    p0[i] = pg[i] * 1 / (3 * pv[i] * pv[i]);

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_hypot(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n0 = MAX2(n1, n2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n0));

  int i0 = 0, i1 = 0, i2 = 0;

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = hypot(p1[i1], p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = hypot(p1[i1], p2[i2]));

          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = hypot(p1[i1], p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(n0, n1, n2, i0, i1, i2,
                p0[i0] = hypot(p1[i1], p2[i2]));

          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else if(n0 == n2)
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_hypot_grad(SEXP a1, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(val))
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n1 = XLENGTH(a1), ng = XLENGTH(grad);

  if(XLENGTH(val) != ng)
    Rf_errorcall(R_NilValue, "argument 'val', and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n1));

  int i1 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pv = REAL(val);
  double *pg = REAL(grad);

  memset(p0, 0, n1 * sizeof(double));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      MATH2(n1, ng, i1, ig,
            p0[i1] += pg[ig] * (p1[i1] / pv[ig]));

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      MATH2(n1, ng, i1, ig,
            p0[i1] += pg[ig] * (p1[i1] / pv[ig]));

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_fma(SEXP a1, SEXP a2, SEXP a3)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(a2))
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");

  if(!Rf_isNumeric(a3))
    Rf_errorcall(R_NilValue, "argument 'a3' must be a numerical vector or array");

  SEXP a0;

  R_len_t n1 = XLENGTH(a1), n2 = XLENGTH(a2), n3 = XLENGTH(a3), n0 = MAX3(n1, n2, n3);

  int i0 = 0, i1 = 0, i2 = 0, i3 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      PROTECT(a0 = Rf_allocVector(REALSXP, n0));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          switch(TYPEOF(a3))
          {
            case REALSXP :
            {
              double *p3 = REAL(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *p3 = INTEGER(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
          }
          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          switch(TYPEOF(a3))
          {
            case REALSXP :
            {
              double *p3 = REAL(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *p3 = INTEGER(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
          }
          break;
        }
      }
      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          PROTECT(a0 = Rf_allocVector(REALSXP, n0));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          switch(TYPEOF(a3))
          {
            case REALSXP :
            {
              double *p3 = REAL(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              int *p3 = INTEGER(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
          }
          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          switch(TYPEOF(a3))
          {
            case REALSXP :
            {
              PROTECT(a0 = Rf_allocVector(REALSXP, n0));

              double *p0 = REAL(a0);
              double *p3 = REAL(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
            case LGLSXP :
            case INTSXP :
            {
              PROTECT(a0 = Rf_allocVector(INTSXP, n0));

              int *p0 = INTEGER(a0);
              int *p3 = INTEGER(a3);

              MATH4(n0, n1, n2, n3, i0, i1, i2, i3,
                    p0[i0] = fma(p1[i1], p2[i2], p3[i3]));

              break;
            }
          }
          break;
        }
      }
      break;
    }
  }

  if(n0 == n1)
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  else if(n0 == n2)
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  else if(n0 == n3)
    SHALLOW_DUPLICATE_ATTRIB(a0, a3);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_exp(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = exp(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = exp(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_exp_grad(SEXP a1, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(val))
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(val) != n || XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1', 'val', and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pv = REAL(val);
  double *pg = REAL(grad);

  for(int i = 0; i < n; i++)
    p0[i] = pg[i] * pv[i];

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_exp2(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = exp2(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = exp2(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_exp2_grad(SEXP a1, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(val))
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(val) != n || XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1', 'val', and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pv = REAL(val);
  double *pg = REAL(grad);

  for(int i = 0; i < n; i++)
    p0[i] = pg[i] * pv[i] * log(2);

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_ln(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = log(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = log(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_ln_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1' and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] / p1[i];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] / p1[i];

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_log2(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = log2(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = log2(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_log2_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1' and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] / (p1[i] * log(2));

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] / (p1[i] * log(2));

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_log10(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = log10(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = log10(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_log10_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1' and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] / (p1[i] * log(10));

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] / (p1[i] * log(10));

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_abs(SEXP a1)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  SEXP a0;

  R_len_t n = XLENGTH(a1);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      for(int i = 0; i < n; i++)
        p0[i] = fabs(p1[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      a0 = PROTECT(Rf_allocVector(INTSXP, n));

      int *p0 = INTEGER(a0);
      int *p1 = INTEGER(a1);

      for(int i = 0; i < n; i++)
        p0[i] = abs(p1[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_abs_grad(SEXP a1, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(a1))
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");

  if(!Rf_isNumeric(val))
    Rf_errorcall(R_NilValue, "argument 'val' must be a numerical vector or array");

  if(!Rf_isReal(grad))
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");

  R_len_t n = XLENGTH(a1);

  if(XLENGTH(val) != n || XLENGTH(grad) != n)
    Rf_errorcall(R_NilValue, "argument 'a1', 'val', and 'grad' have incompatible lengths");

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);
      double *pv = REAL(val);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] * (p1[i] / pv[i]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);
      int *pv = INTEGER(val);

      for(int i = 0; i < n; i++)
        p0[i] = pg[i] * (p1[i] / pv[i]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP sigmoid(SEXP x)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  R_len_t n = XLENGTH(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  double *po = REAL(out);

  const double min = DBL_EPSILON, max = 1 - DBL_EPSILON;

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

  SHALLOW_DUPLICATE_ATTRIB(out, x);

  UNPROTECT(1);

  return out;
}

SEXP sigmoid_grad(SEXP x, SEXP val, SEXP grad)
{
  if(!Rf_isNumeric(x))
  {
    Rf_errorcall(R_NilValue, "argument 'x' must be a numerical vector or array");
  }

  if(!Rf_isReal(val))
  {
    Rf_errorcall(R_NilValue, "argument 'val' must be a real vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  if(XLENGTH(x) != XLENGTH(val) || XLENGTH(val) != XLENGTH(grad))
  {
    Rf_errorcall(R_NilValue, "the lengths of argument 'x' (%d), 'val' (%d), and 'grad' (%d) are incompatible",
                 XLENGTH(x), XLENGTH(val), XLENGTH(grad));
  }

  R_len_t n = XLENGTH(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  double *pv = REAL(val);
  double *pg = REAL(grad);
  double *po = REAL(out);

  for(int i = 0; i < n; i++)
  {
    po[i] = pg[i] * pv[i] * (1 - pv[i]);
  }

  SHALLOW_DUPLICATE_ATTRIB(out, x);

  UNPROTECT(1);

  return out;
}
