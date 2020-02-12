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

#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"
#include "symbols.h"

/*
 * ENUMERATIONS
 */

typedef enum {
  CGGD
} cg_optimizer_type_t;

/*
 * INLINED GET/SET FUNCTIONS
 */

inline SEXP cg_optimizer_parms(SEXP optimizer)
{
  SEXP parms = PROTECT(CG_GET(optimizer, CG_PARMS_SYMBOL));

  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "optimizer has no parameters");
  }

  UNPROTECT(1);

  return parms;
}

inline void cg_optimizer_set_parms(SEXP optimizer, SEXP parms)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  CG_SET(optimizer, CG_PARMS_SYMBOL, parms);
}

inline double cg_optimizer_lr(SEXP optimizer)
{
  SEXP lr = PROTECT(CG_GET(optimizer, CG_LR_SYMBOL));

  if(!IS_SCALAR(lr, REALSXP))
  {
    Rf_errorcall(R_NilValue, "optimizer has no learning rate");
  }

  UNPROTECT(1);

  return REAL(lr)[0];
}

inline void cg_optimizer_set_lr(SEXP optimizer, const double lr)
{
  CG_SET(optimizer, CG_LR_SYMBOL, Rf_ScalarReal(lr));
}

inline cg_optimizer_type_t cg_optimizer_type(SEXP optimizer)
{
  SEXP type = PROTECT(CG_GET(optimizer, CG_TYPE_SYMBOL));

  if(!IS_SCALAR(type, INTSXP))
  {
    Rf_errorcall(R_NilValue, "optimizer has no type");
  }

  UNPROTECT(1);

  return (cg_optimizer_type_t)INTEGER(type)[0];
}

inline void cg_optimizer_set_type(SEXP optimizer, const cg_optimizer_type_t type)
{
  CG_SET(optimizer, CG_TYPE_SYMBOL, Rf_ScalarInteger(type));
}

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_optimizer_step(SEXP optimizer);

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_gd(SEXP parms, SEXP lr);

#endif
