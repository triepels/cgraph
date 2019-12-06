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

#define MAX_LENGTH2(a1, a2) XLENGTH(a1) >= XLENGTH(a2) ? XLENGTH(a1) : XLENGTH(a2)

#define MATH1(a0, a1, i0, i1, expr)\
  do{\
    R_len_t n0_ = XLENGTH(a0);\
    R_len_t n1_ = XLENGTH(a1);\
    if(n0_ == n1_)\
      for(; i0 < n0_; i0++, i1++)\
        expr;\
    else\
      for(; i0 < n0_; i0++,\
          i1 = (++i1 < n1_) ? i1 : 0)\
        expr;\
  }while(0)

#define MATH2(a0, a1, a2, i0, i1, i2, expr)\
  do{\
    R_len_t n0_ = XLENGTH(a0);\
    R_len_t n1_ = XLENGTH(a1);\
    R_len_t n2_ = XLENGTH(a2);\
    if(n0_ == n1_ && n0_ == n2_)\
      for(; i0 < n0_; i0++, i1++, i2++)\
        expr;\
    else if(n1_ == 1)\
      for(; i0 < n0_; i0++, i2++)\
        expr;\
    else if(n2_ == 1)\
      for(; i0 < n0_; i0++, i1++)\
        expr;\
    else\
      for(; i0 < n0_; i0++,\
          i1 = (++i1 < n1_) ? i1 : 0, i2 = (++i2 < n2_) ? i2 : 0)\
        expr;\
  }while(0)

#define MATH3(a0, a1, a2, a3, i0, i1, i2, i3, expr)\
  do{\
    R_len_t n0_ = XLENGTH(a0);\
    R_len_t n1_ = XLENGTH(a1);\
    R_len_t n2_ = XLENGTH(a2);\
    R_len_t n3_ = XLENGTH(a3);\
    if(n0_ == n1_ && n0_ == n2_ && n0_ == n3_)\
      for(; i0 < n0_; i0++, i1++, i2++, i3++)\
        expr;\
    else if(n1_ == 1 && n2_ == 1)\
      for(; i0 < n0_; i0++, i3++)\
        expr;\
    else if(n2_ == 1 && n3_ == 1)\
      for(; i0 < n0_; i0++, i1++)\
        expr;\
    else if(n1_ == 1 && n3_ == 1)\
      for(; i0 < n0_; i0++, i2++)\
        expr;\
    else if(n1_ == 1)\
      for(; i0 < n0_; i0++, i2++, i3++)\
        expr;\
    else if(n2_ == 1)\
      for(; i0 < n0_; i0++, i1++, i3++)\
        expr;\
    else if(n3_ == 1)\
      for(; i0 < n0_; i0++, i1++, i2++)\
        expr;\
    else\
      for(; i0 < n0_; i0++,\
          i1 = (++i1 < n1_) ? i1 : 0, i2 = (++i2 < n2_) ? i2 : 0, i3 = (++i3 < n3_) ? i3 : 0)\
        expr;\
  }while(0)

SEXP cg_math_pos(SEXP a1)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  return Rf_lazy_duplicate(a1);
}

SEXP cg_math_pos_grad(SEXP grad)
{
  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  return Rf_lazy_duplicate(grad);
}

SEXP cg_math_neg(SEXP a1)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_duplicate(a1));

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p0 = REAL(a0);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = -p0[i0];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p0 = INTEGER(a0);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = -p0[i0];

      break;
    }
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_neg_grad(SEXP grad)
{
  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(grad);

  SEXP a0 = PROTECT(Rf_duplicate(grad));

  switch(TYPEOF(grad))
  {
    case REALSXP :
    {
      double *p0 = REAL(a0);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = -p0[i0];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p0 = INTEGER(a0);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = -p0[i0];

      break;
    }
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_add(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  SEXP a0;

  R_len_t n = MAX_LENGTH2(a1, a2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
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
          a0 = PROTECT(Rf_allocVector(REALSXP, n));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          a0 = PROTECT(Rf_allocVector(INTSXP, n));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] + p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n == XLENGTH(a1))
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_add_grad(SEXP a1, SEXP grad)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n * sizeof(double));

  int i0 = 0, ig = 0;

  MATH1(a0, grad, i0, ig,
        p0[i0] += pg[ig]);

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_sub(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  SEXP a0;

  R_len_t n = MAX_LENGTH2(a1, a2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
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
          a0 = PROTECT(Rf_allocVector(REALSXP, n));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          a0 = PROTECT(Rf_allocVector(INTSXP, n));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] - p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n == XLENGTH(a1))
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_sub_grad(SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  memset(p0, 0, n * sizeof(double));

  int i0 = 0, ig = 0;

  MATH1(a0, grad, i0, ig,
        p0[i0] += -pg[ig]);

  SHALLOW_DUPLICATE_ATTRIB(a0, a2);

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_mul(SEXP a1, SEXP a2)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  SEXP a0;

  R_len_t n = MAX_LENGTH2(a1, a2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
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
          a0 = PROTECT(Rf_allocVector(REALSXP, n));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          a0 = PROTECT(Rf_allocVector(INTSXP, n));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] * p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n == XLENGTH(a1))
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_mul_grad1(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a2))
  {
    case REALSXP :
    {
      double *p2 = REAL(a2);

      MATH2(a0, a2, grad, i0, i2, ig,
            p0[i0] = pg[ig] * p2[i2]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p2 = INTEGER(a2);

      MATH2(a0, a2, grad, i0, i2, ig,
            p0[i0] = pg[ig] * p2[i2]);

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i1 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      MATH2(a0, a1, grad, i0, i1, ig,
            p0[i0] = pg[ig] * p1[i1]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      MATH2(a0, a1, grad, i0, i1, ig,
            p0[i0] = pg[ig] * p1[i1]);

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  R_len_t n = MAX_LENGTH2(a1, a2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

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

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
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

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = p1[i1] / p2[i2]);

          break;
        }
      }
      break;
    }
  }

  if(n == XLENGTH(a1))
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_div_grad1(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a2))
  {
    case REALSXP :
    {
      double *p2 = REAL(a2);

      MATH2(a0, a2, grad, i0, i2, ig,
            p0[i0] = pg[ig] / p2[i2]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p2 = INTEGER(a2);

      MATH2(a0, a2, grad, i0, i2, ig,
            p0[i0] = pg[ig] / p2[i2]);

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

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

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

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

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = -pg[ig] * p1[i1] / (p2[i2] * p2[i2]));

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  SEXP a0;

  R_len_t n = MAX_LENGTH2(a1, a2);

  int i0 = 0, i1 = 0, i2 = 0;

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      switch(TYPEOF(a2))
      {
        case REALSXP :
        {
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
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
          a0 = PROTECT(Rf_allocVector(REALSXP, n));

          double *p0 = REAL(a0);
          double *p2 = REAL(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          a0 = PROTECT(Rf_allocVector(INTSXP, n));

          int *p0 = INTEGER(a0);
          int *p2 = INTEGER(a2);

          MATH2(a0, a1, a2, i0, i1, i2,
                p0[i0] = pow(p1[i1], p2[i2]));

          break;
        }
      }
      break;
    }
  }

  if(n == XLENGTH(a1))
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a1);
  }
  else
  {
    SHALLOW_DUPLICATE_ATTRIB(a0, a2);
  }

  UNPROTECT(1);

  return a0;
}

SEXP cg_math_pow_grad1(SEXP a1, SEXP a2, SEXP grad)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

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

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

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

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * p2[i2] * pow(p1[i1], p2[i2] - 1));

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isNumeric(a2))
  {
    Rf_errorcall(R_NilValue, "argument 'a2' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a2);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i1 = 0, i2 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

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

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

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

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

          break;
        }
        case LGLSXP :
        case INTSXP :
        {
          int *p2 = INTEGER(a2);

          MATH3(a0, a1, a2, grad, i0, i1, i2, ig,
                p0[i0] = pg[ig] * pow(p1[i1], p2[i2]) * log(p1[i1]));

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  SEXP a0;

  R_len_t n = XLENGTH(a1);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      a0 = PROTECT(Rf_allocVector(REALSXP, n));

      double *p0 = REAL(a0);
      double *p1 = REAL(a1);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = p1[i0] * p1[i0];

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      a0 = PROTECT(Rf_allocVector(INTSXP, n));

      int *p0 = INTEGER(a0);
      int *p1 = INTEGER(a1);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = p1[i0] * p1[i0];

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  if(!Rf_isReal(grad))
  {
    Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  int i0 = 0, i1 = 0, ig = 0;

  double *p0 = REAL(a0);
  double *pg = REAL(grad);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      MATH2(a0, a1, grad, i0, i1, ig,
            p0[i0] = 2 * pg[ig] * p1[i1]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      MATH2(a0, a1, grad, i0, i1, ig,
            p0[i0] = 2 * pg[ig] * p1[i1]);

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
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  R_len_t n = XLENGTH(a1);

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, n));

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = sqrt(p1[i0]);

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      for(int i0 = 0; i0 < n; i0++)
        p0[i0] = sqrt(p1[i0]);

      break;
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(a0, a1);

  UNPROTECT(1);

  return a0;
}

SEXP math_sin(SEXP a1)
{
  if(!Rf_isNumeric(a1))
  {
    Rf_errorcall(R_NilValue, "argument 'a1' must be a numerical vector or array");
  }

  SEXP a0 = PROTECT(Rf_allocVector(REALSXP, XLENGTH(a1)));

  int i0 = 0, i1 = 0;

  double *p0 = REAL(a0);

  switch(TYPEOF(a1))
  {
    case REALSXP :
    {
      double *p1 = REAL(a1);

      MATH1(a0, a1, i0, i1,
            p0[i0] = sin(p1[i1]));

      break;
    }
    case LGLSXP :
    case INTSXP :
    {
      int *p1 = INTEGER(a1);

      MATH1(a0, a1, i0, i1,
            p0[i0] = sin(p1[i1]));

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
