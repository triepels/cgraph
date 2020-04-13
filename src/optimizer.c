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

extern inline SEXP cg_opt_buffer0(SEXP opt);

extern inline void cg_opt_set_buffer0(SEXP opt, SEXP buffer);

extern inline SEXP cg_opt_buffer1(SEXP opt);

extern inline void cg_opt_set_buffer1(SEXP opt, SEXP buffer);

extern inline SEXP cg_opt_parms(SEXP opt);

extern inline void cg_opt_set_parms(SEXP opt, SEXP parms);

extern inline double cg_opt_eps(SEXP opt);

extern inline void cg_opt_set_eps(SEXP opt, const double eps);

extern inline double* cg_opt_betas(SEXP opt);

extern inline void cg_opt_set_betas(SEXP opt, const double beta1, const double beta2);

extern inline double* cg_opt_gammas(SEXP opt);

extern inline void cg_opt_set_gammas(SEXP opt, const double gamma1, const double gamma2);

extern inline double cg_opt_gamma(SEXP opt);

extern inline void cg_opt_set_gamma(SEXP opt, const double gamma);

extern inline double cg_opt_eta(SEXP opt);

extern inline void cg_opt_set_eta(SEXP opt, const double eta);

extern inline cg_opt_type_t cg_opt_type(SEXP opt);

extern inline void cg_opt_set_type(SEXP opt, const cg_opt_type_t type);

/*
 * PRIVATE FUNCTIONS
 */

static SEXP cg_buffer(SEXP parms)
{
  R_xlen_t n = XLENGTH(parms);

  SEXP buffer = PROTECT(Rf_allocVector(VECSXP, n));

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(!cg_is(parm, "cg_node"))
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isReal(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP state = PROTECT(Rf_duplicate(value));

    memset(REAL(state), 0, XLENGTH(value) * sizeof(double));

    SET_VECTOR_ELT(buffer, i, state);

    UNPROTECT(2);
  }

  SHALLOW_DUPLICATE_ATTRIB(buffer, parms);

  UNPROTECT(1);

  return buffer;
}

static inline void cg_opt_gd_step(SEXP opt)
{
  SEXP parms = PROTECT(cg_opt_parms(opt));

  const double eta = cg_opt_eta(opt);

  R_len_t n = XLENGTH(parms);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(TYPEOF(parm) != ENVSXP)
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isReal(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP grad = PROTECT(cg_node_grad(parm));

    if(!Rf_isReal(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(parm));
    }

    R_len_t m = XLENGTH(value);

    if(m != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of length %d for node '%s'",
                   XLENGTH(grad), cg_node_name(parm));
    }

    double *pv = REAL(value);
    double *pg = REAL(grad);

    for(int i = 0; i < m; i++)
    {
      pv[i] -= eta * pg[i];
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);
}

static inline void cg_opt_gd_momentum_step(SEXP opt)
{
  SEXP parms = PROTECT(cg_opt_parms(opt));

  SEXP buffer0 = PROTECT(cg_opt_buffer0(opt));

  R_len_t n = XLENGTH(parms);

  if(n != XLENGTH(buffer0))
  {
    Rf_errorcall(R_NilValue, "cannot process first moments buffer of length %d",
                 XLENGTH(buffer0));
  }

  const double eta = cg_opt_eta(opt);

  const double gamma = cg_opt_gamma(opt);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(TYPEOF(parm) != ENVSXP)
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP state0 = VECTOR_ELT(buffer0, i);

    if(!Rf_isReal(state0))
    {
      Rf_errorcall(R_NilValue, "cannot process first moment of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(state0)), cg_node_name(parm));
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isReal(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP grad = PROTECT(cg_node_grad(parm));

    if(!Rf_isReal(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(parm));
    }

    R_len_t m = XLENGTH(value);

    if(m != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of length %d for node '%s'",
                   XLENGTH(grad), cg_node_name(parm));
    }

    if(m != XLENGTH(state0))
    {
      Rf_errorcall(R_NilValue, "cannot process first moment of length %d for node '%s'",
                   XLENGTH(state0), cg_node_name(parm));
    }

    double *p0 = REAL(state0);
    double *pv = REAL(value);
    double *pg = REAL(grad);

    for(int i = 0; i < m; i++)
    {
      p0[i] = gamma * p0[i] + eta * pg[i];

      pv[i] -= p0[i];
    }

    UNPROTECT(2);
  }

  UNPROTECT(2);
}

static inline void cg_opt_adagrad_step(SEXP opt)
{
  SEXP parms = PROTECT(cg_opt_parms(opt));

  SEXP buffer1 = PROTECT(cg_opt_buffer1(opt));

  R_len_t n = XLENGTH(parms);

  if(n != XLENGTH(buffer1))
  {
    Rf_errorcall(R_NilValue, "cannot process second moments buffer of length %d",
                 XLENGTH(buffer1));
  }

  const double eta = cg_opt_eta(opt);

  const double eps = cg_opt_eps(opt);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(TYPEOF(parm) != ENVSXP)
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP state1 = VECTOR_ELT(buffer1, i);

    if(!Rf_isReal(state1))
    {
      Rf_errorcall(R_NilValue, "cannot process second moment of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(state1)), cg_node_name(parm));
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isReal(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP grad = PROTECT(cg_node_grad(parm));

    if(!Rf_isReal(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(parm));
    }

    R_len_t m = XLENGTH(value);

    if(m != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of length %d for node '%s'",
                   XLENGTH(grad), cg_node_name(parm));
    }

    if(m != XLENGTH(state1))
    {
      Rf_errorcall(R_NilValue, "cannot process second moment of length %d for node '%s'",
                   XLENGTH(state1), cg_node_name(parm));
    }

    double *p1 = REAL(state1);
    double *pv = REAL(value);
    double *pg = REAL(grad);

    for(int i = 0; i < m; i++)
    {
      p1[i] += pg[i] * pg[i];

      pv[i] -= eta / sqrt(p1[i] + eps) * pg[i];
    }

    UNPROTECT(2);
  }

  UNPROTECT(2);
}

static inline void cg_opt_rmsprop_step(SEXP opt)
{
  SEXP parms = PROTECT(cg_opt_parms(opt));

  SEXP buffer1 = PROTECT(cg_opt_buffer1(opt));

  R_len_t n = XLENGTH(parms);

  if(n != XLENGTH(buffer1))
  {
    Rf_errorcall(R_NilValue, "cannot process second moments buffer of length %d",
                 XLENGTH(buffer1));
  }

  const double eta = cg_opt_eta(opt);

  const double gamma = cg_opt_gamma(opt);

  const double eps = cg_opt_eps(opt);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(TYPEOF(parm) != ENVSXP)
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP state1 = VECTOR_ELT(buffer1, i);

    if(!Rf_isReal(state1))
    {
      Rf_errorcall(R_NilValue, "cannot process second moment of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(state1)), cg_node_name(parm));
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isReal(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP grad = PROTECT(cg_node_grad(parm));

    if(!Rf_isReal(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(parm));
    }

    R_len_t m = XLENGTH(value);

    if(m != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of length %d for node '%s'",
                   XLENGTH(grad), cg_node_name(parm));
    }

    if(m != XLENGTH(state1))
    {
      Rf_errorcall(R_NilValue, "cannot process second moment of length %d for node '%s'",
                   XLENGTH(state1), cg_node_name(parm));
    }

    double *p1 = REAL(state1);
    double *pv = REAL(value);
    double *pg = REAL(grad);

    for(int i = 0; i < m; i++)
    {
      p1[i] = gamma * p1[i] + (1 - gamma) * pg[i] * pg[i];

      pv[i] -= eta / sqrt(p1[i] + eps) * pg[i];
    }

    UNPROTECT(2);
  }

  UNPROTECT(2);
}

static inline void cg_opt_adam_step(SEXP opt)
{
  SEXP parms = PROTECT(cg_opt_parms(opt));

  SEXP buffer0 = PROTECT(cg_opt_buffer0(opt));

  R_len_t n = XLENGTH(parms);

  if(n != XLENGTH(buffer0))
  {
    Rf_errorcall(R_NilValue, "cannot process first moments buffer of length %d",
                 XLENGTH(buffer0));
  }

  SEXP buffer1 = PROTECT(cg_opt_buffer1(opt));

  if(n != XLENGTH(buffer1))
  {
    Rf_errorcall(R_NilValue, "cannot process second moments buffer of length %d",
                 XLENGTH(buffer1));
  }

  const double eta = cg_opt_eta(opt);

  const double *beta = cg_opt_betas(opt);

  const double eps = cg_opt_eps(opt);

  double *gamma = cg_opt_gammas(opt);

  for(int i = 0; i < n; i++)
  {
    SEXP parm = VECTOR_ELT(parms, i);

    if(TYPEOF(parm) != ENVSXP)
    {
      Rf_errorcall(R_NilValue, "argument 'parms' has an invalid parameter at index %d", i + 1);
    }

    SEXP state0 = VECTOR_ELT(buffer0, i);

    if(!Rf_isReal(state0))
    {
      Rf_errorcall(R_NilValue, "cannot process first moment of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(state0)), cg_node_name(parm));
    }

    SEXP state1 = VECTOR_ELT(buffer1, i);

    if(!Rf_isReal(state1))
    {
      Rf_errorcall(R_NilValue, "cannot process second moment of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(state1)), cg_node_name(parm));
    }

    SEXP value = PROTECT(cg_node_value(parm));

    if(!Rf_isReal(value))
    {
      Rf_errorcall(R_NilValue, "cannot process value of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(value)), cg_node_name(parm));
    }

    SEXP grad = PROTECT(cg_node_grad(parm));

    if(!Rf_isReal(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of type '%s' for node '%s'",
                   Rf_type2char(TYPEOF(grad)), cg_node_name(parm));
    }

    R_len_t m = XLENGTH(value);

    if(m != XLENGTH(grad))
    {
      Rf_errorcall(R_NilValue, "cannot process gradient of length %d for node '%s'",
                   XLENGTH(grad), cg_node_name(parm));
    }

    if(m != XLENGTH(state0))
    {
      Rf_errorcall(R_NilValue, "cannot process first moment of length %d for node '%s'",
                   XLENGTH(state0), cg_node_name(parm));
    }

    if(m != XLENGTH(state1))
    {
      Rf_errorcall(R_NilValue, "cannot process second moment of length %d for node '%s'",
                   XLENGTH(state1), cg_node_name(parm));
    }

    double *p0 = REAL(state0);
    double *p1 = REAL(state1);
    double *pv = REAL(value);
    double *pg = REAL(grad);

    for(int i = 0; i < m; i++)
    {
      p0[i] = beta[0] * p0[i] + (1 - beta[0]) * pg[i];
      p1[i] = beta[1] * p1[i] + (1 - beta[1]) * pg[i] * pg[i];

      pv[i] -= eta / (sqrt(p1[i] / (1 - gamma[1])) + eps) * (p0[i] / (1 - gamma[0]));
    }

    UNPROTECT(2);
  }

  gamma[0] *= beta[0];
  gamma[1] *= beta[1];

  UNPROTECT(3);
}

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_opt_step(SEXP opt)
{
  if(TYPEOF(opt) != ENVSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'opt' must be a cg_opt object");
  }

  switch(cg_opt_type(opt))
  {
    case CGSGD :
      cg_opt_gd_step(opt);
      break;
    case CGGDM :
      cg_opt_gd_momentum_step(opt);
      break;
    case CGADG :
      cg_opt_adagrad_step(opt);
      break;
    case CGRMS :
      cg_opt_rmsprop_step(opt);
      break;
    case CGADM :
      cg_opt_adam_step(opt);
      break;
    default :
      Rf_errorcall(R_NilValue, "optimizer is not yet implemented");
  }

  return R_NilValue;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_opt_gd(SEXP parms, SEXP eta)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  if(!IS_SCALAR(eta, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eta' must be a real scalar");
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

  SEXP opt = PROTECT(cg_class("cg_opt"));

  CG_SET(opt, CG_ETA_SYMBOL, eta);

  CG_SET(opt, CG_PARMS_SYMBOL, parms);

  CG_SET(opt, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGSGD));

  UNPROTECT(1);

  return opt;
}

SEXP cg_opt_gd_momentum(SEXP parms, SEXP eta, SEXP gamma)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  if(!IS_SCALAR(eta, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eta' must be a real scalar");
  }

  if(!IS_SCALAR(gamma, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'gamma' must be a real scalar");
  }

  SEXP opt = PROTECT(cg_class("cg_opt"));

  CG_SET(opt, CG_BUFFER0_SYMBOL, cg_buffer(parms));

  CG_SET(opt, CG_GAMMA_SYMBOL, gamma);

  CG_SET(opt, CG_ETA_SYMBOL, eta);

  CG_SET(opt, CG_PARMS_SYMBOL, parms);

  CG_SET(opt, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGGDM));

  UNPROTECT(1);

  return opt;
}

SEXP cg_opt_adagrad(SEXP parms, SEXP eta, SEXP eps)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  if(!IS_SCALAR(eta, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eta' must be a real scalar");
  }

  if(!IS_SCALAR(eps, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eps' must be a real scalar");
  }

  SEXP opt = PROTECT(cg_class("cg_opt"));

  CG_SET(opt, CG_BUFFER1_SYMBOL, cg_buffer(parms));

  CG_SET(opt, CG_EPS_SYMBOL, eps);

  CG_SET(opt, CG_ETA_SYMBOL, eta);

  CG_SET(opt, CG_PARMS_SYMBOL, parms);

  CG_SET(opt, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGADG));

  UNPROTECT(1);

  return opt;
}

SEXP cg_opt_rmsprop(SEXP parms, SEXP eta, SEXP gamma, SEXP eps)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  if(!IS_SCALAR(eta, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eta' must be a real scalar");
  }

  if(!IS_SCALAR(gamma, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'gamma' must be a real scalar");
  }

  if(!IS_SCALAR(eps, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eps' must be a real scalar");
  }

  SEXP opt = PROTECT(cg_class("cg_opt"));

  CG_SET(opt, CG_BUFFER1_SYMBOL, cg_buffer(parms));

  CG_SET(opt, CG_EPS_SYMBOL, eps);

  CG_SET(opt, CG_GAMMA_SYMBOL, gamma);

  CG_SET(opt, CG_ETA_SYMBOL, eta);

  CG_SET(opt, CG_PARMS_SYMBOL, parms);

  CG_SET(opt, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGRMS));

  UNPROTECT(1);

  return opt;
}

SEXP cg_opt_adam(SEXP parms, SEXP eta, SEXP betas, SEXP eps)
{
  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "argument 'parms' must be a list of parameters");
  }

  if(!IS_SCALAR(eta, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eta' must be a real scalar");
  }

  if(TYPEOF(betas) != REALSXP || XLENGTH(betas) != 2)
  {
    Rf_errorcall(R_NilValue, "argument 'betas' must be a real vector of length 2");
  }

  if(!IS_SCALAR(eps, REALSXP))
  {
    Rf_errorcall(R_NilValue, "argument 'eps' must be a real scalar");
  }

  SEXP opt = PROTECT(cg_class("cg_opt"));

  CG_SET(opt, CG_BUFFER1_SYMBOL, cg_buffer(parms));

  CG_SET(opt, CG_BUFFER0_SYMBOL, cg_buffer(parms));

  CG_SET(opt, CG_EPS_SYMBOL, eps);

  CG_SET(opt, CG_GAMMAS_SYMBOL, Rf_duplicate(betas));

  CG_SET(opt, CG_BETAS_SYMBOL, betas);

  CG_SET(opt, CG_ETA_SYMBOL, eta);

  CG_SET(opt, CG_PARMS_SYMBOL, parms);

  CG_SET(opt, CG_TYPE_SYMBOL, Rf_ScalarInteger(CGADM));

  UNPROTECT(1);

  return opt;
}
