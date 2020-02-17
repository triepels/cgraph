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
  CGSGD,  /* (Stochastic) Gradient Desecnt */
  CGGDM,  /* Gradient Descent with Momentum */
  CGNAG,  /* Nestrov Accelerated Gradient */
  CGADG,  /* Adagrad */
  CGADD,  /* Adadelta */
  CGRMS,  /* Root Mean Square Propagation */
  CGADM   /* ADAM */
} cg_optimizer_type_t;

/*
 * INLINED GET/SET FUNCTIONS
 */

inline SEXP cg_optimizer_buffer0(SEXP optimizer)
{
  SEXP buffer = PROTECT(CG_GET(optimizer, CG_BUFFER0_SYMBOL));

  if(TYPEOF(buffer) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "optimizer has no first moments buffer");
  }

  UNPROTECT(1);

  return buffer;
}

inline void cg_optimizer_set_buffer0(SEXP optimizer, SEXP buffer)
{
  if(TYPEOF(buffer) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'buffer' must be a list of buffered states");
  }

  CG_SET(optimizer, CG_BUFFER0_SYMBOL, buffer);
}

inline SEXP cg_optimizer_buffer1(SEXP optimizer)
{
  SEXP buffer = PROTECT(CG_GET(optimizer, CG_BUFFER1_SYMBOL));

  if(TYPEOF(buffer) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "optimizer has no second moments buffer");
  }

  UNPROTECT(1);

  return buffer;
}

inline void cg_optimizer_set_buffer1(SEXP optimizer, SEXP buffer)
{
  if(TYPEOF(buffer) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'buffer' must be a list of buffered states");
  }

  CG_SET(optimizer, CG_BUFFER1_SYMBOL, buffer);
}

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

inline double cg_optimizer_eps(SEXP optimizer)
{
  SEXP eps = PROTECT(CG_GET(optimizer, CG_EPS_SYMBOL));

  if(!IS_SCALAR(eps, REALSXP))
  {
    Rf_errorcall(R_NilValue, "optimizer has no error term");
  }

  UNPROTECT(1);

  return REAL(eps)[0];
}

inline void cg_optimizer_set_eps(SEXP optimizer, const double eps)
{
  CG_SET(optimizer, CG_EPS_SYMBOL, Rf_ScalarReal(eps));
}

inline double* cg_optimizer_betas(SEXP optimizer)
{
  SEXP betas = PROTECT(CG_GET(optimizer, CG_BETAS_SYMBOL));

  if(TYPEOF(betas) != REALSXP || XLENGTH(betas) != 2)
  {
    Rf_errorcall(R_NilValue, "optimizer has no beta coefficients");
  }

  UNPROTECT(1);

  return REAL(betas);
}

inline void cg_optimizer_set_betas(SEXP optimizer, const double beta1, const double beta2)
{
  SEXP betas = PROTECT(Rf_allocVector(REALSXP, 2));

  REAL(betas)[0] = beta1;
  REAL(betas)[1] = beta2;

  CG_SET(optimizer, CG_BETAS_SYMBOL, betas);

  UNPROTECT(1);
}

inline double* cg_optimizer_gammas(SEXP optimizer)
{
  SEXP gammas = PROTECT(CG_GET(optimizer, CG_GAMMAS_SYMBOL));

  if(TYPEOF(gammas) != REALSXP || XLENGTH(gammas) != 2)
  {
    Rf_errorcall(R_NilValue, "optimizer has no gamma coefficients");
  }

  UNPROTECT(1);

  return REAL(gammas);
}

inline void cg_optimizer_set_gammas(SEXP optimizer, const double gamma1, const double gamma2)
{
  SEXP gammas = PROTECT(Rf_allocVector(REALSXP, 2));

  REAL(gammas)[0] = gamma1;
  REAL(gammas)[1] = gamma2;

  CG_SET(optimizer, CG_GAMMAS_SYMBOL, gammas);

  UNPROTECT(1);
}

inline double cg_optimizer_gamma(SEXP optimizer)
{
  SEXP gamma = PROTECT(CG_GET(optimizer, CG_GAMMA_SYMBOL));

  if(!IS_SCALAR(gamma, REALSXP))
  {
    Rf_errorcall(R_NilValue, "optimizer has no momentum rate");
  }

  UNPROTECT(1);

  return REAL(gamma)[0];
}

inline void cg_optimizer_set_gamma(SEXP optimizer, const double gamma)
{
  CG_SET(optimizer, CG_GAMMA_SYMBOL, Rf_ScalarReal(gamma));
}

inline double cg_optimizer_eta(SEXP optimizer)
{
  SEXP eta = PROTECT(CG_GET(optimizer, CG_ETA_SYMBOL));

  if(!IS_SCALAR(eta, REALSXP))
  {
    Rf_errorcall(R_NilValue, "optimizer has no learning rate");
  }

  UNPROTECT(1);

  return REAL(eta)[0];
}

inline void cg_optimizer_set_eta(SEXP optimizer, const double eta)
{
  CG_SET(optimizer, CG_ETA_SYMBOL, Rf_ScalarReal(eta));
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

SEXP cg_gd(SEXP parms, SEXP eta);

SEXP cg_gd_momentum(SEXP parms, SEXP eta, SEXP gamma);

SEXP cg_adagrad(SEXP parms, SEXP eta, SEXP eps);

SEXP cg_rmsprop(SEXP parms, SEXP eta, SEXP gamma, SEXP eps);

SEXP cg_adam(SEXP parms, SEXP eta, SEXP betas, SEXP eps);

#endif
