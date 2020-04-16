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

#ifndef NODE_H
#define NODE_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "class.h"
#include "symbols.h"

/*
 * ENUMERATIONS
 */

typedef enum {
    CGCST = 0, /* Constant */
    CGPRM = 1, /* Parameter */
    CGIPT = 2, /* Input */
    CGDOP = 3, /* Differentiable Operator */
    CGNOP = 4  /* Non-differentiable Operator */
} cg_node_type_t;

/*
 * INLINED GET/SET FUNCTIONS
 */

inline SEXP cg_node_name(SEXP node)
{
    SEXP name = PROTECT(CG_GET(node, CG_NAME_SYMBOL));

    if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
    {
        Rf_errorcall(R_NilValue, "node has no name");
    }

    UNPROTECT(1);

    return name;
}

inline int cg_node_id(SEXP node)
{
    SEXP id = PROTECT(CG_GET(node, CG_ID_SYMBOL));

    if(!IS_SCALAR(id, INTSXP))
    {
        Rf_errorcall(R_NilValue, "node '%s' has no id", cg_node_name(node));
    }

    UNPROTECT(1);

    return INTEGER(id)[0];
}

inline const char* cg_node_name_char(SEXP node)
{
    SEXP name = PROTECT(CG_GET(node, CG_NAME_SYMBOL));

    if(Rf_isNull(name))
    {
        char *name = R_alloc(1, 32 * sizeof(char));

        sprintf(name, "v%d", cg_node_id(node));

        UNPROTECT(1);

        return name;
    }

    UNPROTECT(1);

    return CHAR(STRING_ELT(name, 0));
}

inline void cg_node_set_name(SEXP node, SEXP name)
{
    if(!Rf_isNull(name) && !IS_SCALAR(name, STRSXP))
    {
        Rf_errorcall(R_NilValue, "argument 'name' must be a character scalar");
    }

    CG_SET(node, CG_NAME_SYMBOL, name);
}

inline void cg_node_set_id(SEXP node, const int id)
{
    if(id < 1)
    {
        Rf_errorcall(R_NilValue, "argument 'id' must be a positive integer");
    }

    CG_SET(node, CG_ID_SYMBOL, Rf_ScalarInteger(id));
}

inline cg_node_type_t cg_node_type(SEXP node)
{
    SEXP type = PROTECT(CG_GET(node, CG_TYPE_SYMBOL));

    if(!IS_SCALAR(type, INTSXP))
    {
        Rf_errorcall(R_NilValue, "node '%s' has no type", cg_node_name(node));
    }

    UNPROTECT(1);

    return (cg_node_type_t)INTEGER(type)[0];
}

inline void cg_node_set_type(SEXP node, const cg_node_type_t type)
{
    CG_SET(node, CG_TYPE_SYMBOL, Rf_ScalarInteger(type));
}

inline SEXP cg_node_inputs(SEXP node)
{
    SEXP inputs = PROTECT(CG_GET(node, CG_INPUTS_SYMBOL));

    if(TYPEOF(inputs) != VECSXP)
    {
        Rf_errorcall(R_NilValue, "node '%s' has no inputs", cg_node_name(node));
    }

    UNPROTECT(1);

    return inputs;
}

inline void cg_node_set_inputs(SEXP node, SEXP inputs)
{
    if(TYPEOF(inputs) != VECSXP)
    {
        Rf_errorcall(R_NilValue, "argument 'inputs' must be a list of inputs");
    }

    CG_SET(node, CG_INPUTS_SYMBOL, inputs);
}

inline SEXP cg_node_value(SEXP node)
{
    SEXP value = PROTECT(CG_GET(node, CG_VALUE_SYMBOL));

    if(value == R_UnboundValue)
    {
        Rf_errorcall(R_NilValue, "node '%s' has no value", cg_node_name(node));
    }

    UNPROTECT(1);

    return value;
}

inline void cg_node_set_value(SEXP node, SEXP value)
{
    CG_SET(node, CG_VALUE_SYMBOL, value);
}

inline SEXP cg_node_grad(SEXP node)
{
    SEXP grad = PROTECT(CG_GET(node, CG_GRAD_SYMBOL));

    if(!Rf_isNull(grad) && TYPEOF(grad) != REALSXP)
    {
        Rf_errorcall(R_NilValue, "node '%s' has no gradient", cg_node_name(node));
    }

    UNPROTECT(1);

    return grad;
}

inline void cg_node_set_grad(SEXP node, SEXP grad)
{
    if(TYPEOF(grad) != REALSXP)
    {
        Rf_errorcall(R_NilValue, "argument 'grad' must be a real vector or array");
    }

    CG_SET(node, CG_GRAD_SYMBOL, grad);
}

inline SEXP cg_node_function(SEXP node)
{
    SEXP function = PROTECT(CG_GET(node, CG_FUN_SYMBOL));

    if(!cg_is(function, "cg_function"))
    {
        Rf_errorcall(R_NilValue, "node '%s' has no function", cg_node_name(node));
    }

    UNPROTECT(1);

    return function;
}

inline void cg_node_set_function(SEXP node, SEXP function)
{
    if(!cg_is(function, "cg_function"))
    {
        Rf_errorcall(R_NilValue, "argument 'function' must be a cg_function object");
    }

    CG_SET(node, CG_FUN_SYMBOL, function);
}

/*
 * PUBLIC FUNCTIONS
 */

void cg_node_zero_grad(SEXP node);

void cg_node_init_grad(SEXP node, SEXP index);

void cg_node_forward(SEXP node);

void cg_node_backward(SEXP node);

SEXP cg_node_print(SEXP node);

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_constant(SEXP value, SEXP name);

SEXP cg_parameter(SEXP value, SEXP name);

SEXP cg_input(SEXP name);

SEXP cg_operator(SEXP function, SEXP inputs, SEXP name);

#endif
