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

#include "node.h"
#include "optimizer.h"

/*
 * INLINED GET/SET FUNCTIONS
 */

extern inline SEXP cg_optimizer_parms(SEXP optimizer);

extern inline void cg_optimizer_set_parms(SEXP optimizer, SEXP parms);

extern inline double cg_optimizer_lr(SEXP optimizer);

extern inline void cg_optimizer_set_lr(SEXP optimizer, const double eta);

extern inline cg_optimizer_type_t cg_optimizer_type(SEXP optimizer);

extern inline void cg_optimizer_set_type(SEXP optimizer, const cg_optimizer_type_t type);

/*
 * PRIVATE FUNCTIONS
 */

static inline void cg_gd_step(SEXP optimizer)
{
  SEXP parms = PROTECT(cg_optimizer_parms(optimizer));

  const double lr = cg_optimizer_lr(optimizer);

  R_len_t n = XLENGTH(parms);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(TYPEOF(parm) != ENVSXP)
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isNumeric(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP grad = PROTECT(cg_node_grad(parm));

    if(!Rf_isNumeric(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(parm));
    }

    R_len_t m = XLENGTH(value);

    if(m != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process value and gradient of length %d and %d respectively for node '%s'",
                   m, XLENGTH(grad), cg_node_name(parm));
    }

    switch(TYPEOF(value))
    {
      case REALSXP :
      {
        double *pv = REAL(value);

        switch(TYPEOF(grad))
        {
          case REALSXP :
          {
            double *pg = REAL(grad);

            for(int i = 0; i < m; i++)
            {
              pv[i] -= lr * pg[i];
            }

            break;
          }
          case INTSXP :
          case LGLSXP :
          {
            int *pg = INTEGER(grad);

            for(int i = 0; i < m; i++)
            {
              pv[i] -= lr * pg[i];
            }

            break;
          }
        }

        break;
      }
      case LGLSXP :
      case INTSXP :
      {
        int *pv = INTEGER(value);

        switch(TYPEOF(grad))
        {
          case REALSXP :
          {
            double *pg = REAL(grad);

            for(int i = 0; i < m; i++)
            {
              pv[i] -= lr * pg[i];
            }

            break;
          }
          case INTSXP :
          case LGLSXP :
          {
            int *pg = INTEGER(grad);

            for(int i = 0; i < m; i++)
            {
              pv[i] -= lr * pg[i];
            }

            break;
          }
        }

        break;
      }
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);
}

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_optimizer_step(SEXP optimizer)
{
  if(TYPEOF(optimizer) != ENVSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'optimizer' must be a cg_optimizer object");
  }

  switch(cg_optimizer_type(optimizer))
  {
    case CGGD :
      cg_gd_step(optimizer);
      break;
  }

  return R_NilValue;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_gd(SEXP parms, SEXP lr)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  if(!IS_SCALAR(lr, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'lr' must be a real scalar");
  }

  R_xlen_t n = XLENGTH(parms);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(!cg_is(parm, "cg_node"))
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }
  }

  SEXP optimizer = PROTECT(cg_class("cg_optimizer"));

  CG_SET(optimizer, CG_LR_SYMBOL, lr);

  CG_SET(optimizer, CG_PARMS_SYMBOL, parms);

  CG_SET(optimizer, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGGD));

  UNPROTECT(1);

  return optimizer;
}
