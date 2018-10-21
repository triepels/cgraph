/*
Copyright 2018 Ron Triepels

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

#include <R.h>
#include <Rinternals.h>

#include "stack.h"

#define R_NO_REMAP

#define CGCST 0
#define CGIPT 1
#define CGPRM 2
#define CGOPR 3

#define CG_NodesSymbol Rf_install("nodes")
#define CG_ValuesSymbol Rf_install("values")

#define CG_TypeSymbol Rf_install("type")
#define CG_CallSymbol Rf_install("call")
#define CG_GradsSymbol Rf_install("grads")
#define CG_ParentsSymbol Rf_install("parents")
#define CG_ChilderenSymbol Rf_install("childeren")

static SEXP NewEnv(SEXP enclos)
{
  SEXP env = PROTECT(Rf_allocSExp(ENVSXP));

  SET_FRAME(env, R_NilValue);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);

  if(Rf_isNull(enclos) || !Rf_isEnvironment(enclos))
  {
    SET_ENCLOS(env, R_GlobalEnv);
  }
  else
  {
    SET_ENCLOS(env, enclos);
  }

  UNPROTECT(1);

  return env;
}

SEXP cgraph(SEXP graph, SEXP nodes, SEXP values)
{
  int n = 0;

  if(Rf_isNull(graph) || !Rf_isEnvironment(graph))
  {
    graph = PROTECT(NewEnv(R_EmptyEnv));

    n++;
  }

  if(Rf_isNull(nodes) || !Rf_isEnvironment(nodes))
  {
    nodes = PROTECT(NewEnv(R_EmptyEnv));

    n++;
  }

  if(Rf_isNull(values) || !Rf_isEnvironment(values))
  {
    values = PROTECT(NewEnv(R_BaseEnv));

    n++;
  }

  Rf_defineVar(CG_NodesSymbol, nodes, graph);
  Rf_defineVar(CG_ValuesSymbol, values, graph);

  Rf_setAttrib(graph, R_ClassSymbol, Rf_mkString("cgraph"));

  UNPROTECT(n);

  return graph;
}

int is_cgraph(SEXP x)
{
  SEXP x_class = Rf_getAttrib(x, R_ClassSymbol);

  if(!Rf_isEnvironment(x))
  {
    return FALSE;
  }

  if(!Rf_isString(x_class))
  {
    return FALSE;
  }

  if(strcmp(CHAR(Rf_asChar(x_class)), "cgraph") != 0)
  {
    return FALSE;
  }

  return TRUE;
}

int is_cg_node(SEXP x)
{
  SEXP x_class = Rf_getAttrib(x, R_ClassSymbol);

  if(!Rf_isString(x) || Rf_asChar(x) == R_BlankString)
  {
    return FALSE;
  }

  if(!Rf_isString(x_class))
  {
    return FALSE;
  }

  if(strcmp(CHAR(Rf_asChar(x_class)), "cg.node") != 0)
  {
    return FALSE;
  }

  return TRUE;
}

static SEXP cg_get_nodes(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    Rf_errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, Rf_install("nodes")));

  if(!Rf_isEnvironment(nodes))
  {
    Rf_errorcall(R_NilValue, "cannot find valid nodes object in cgraph object environment");
  }

  UNPROTECT(1);

  return nodes;
}

static SEXP cg_get_values(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    Rf_errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP values = PROTECT(Rf_findVarInFrame(graph, CG_ValuesSymbol));

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "cannot find valid values object in cgraph object environment");
  }

  UNPROTECT(1);

  return values;
}

const char* cg_get_name(SEXP node)
{
  if(!Rf_isString(node) || Rf_asChar(node) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "node has an invalid name");
  }

  return CHAR(STRING_ELT(node, 0));
}

void cg_set_name(SEXP node, const char* name)
{
  if(!Rf_isString(node))
  {
    Rf_errorcall(R_NilValue, "node has an invalid name");
  }

  if(strcmp(name, "") == 0)
  {
    Rf_errorcall(R_NilValue, "the name of a node must be a non-blank character scalar");
  }

  if(strcmp(name, "grad") == 0)
  {
    Rf_errorcall(R_NilValue, "the name of a node cannot be 'grad' because this is a reserved word");
  }

  SET_STRING_ELT(node, 0, Rf_mkChar(name));
}

SEXP cg_get_symbol(SEXP node)
{
  if(!Rf_isString(node) || Rf_asChar(node) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "node has an invalid name");
  }

  return Rf_install(CHAR(Rf_asChar(node)));
}

int cg_get_type(SEXP node)
{
  SEXP node_type = Rf_getAttrib(node, CG_TypeSymbol);
  // SIZE = 0 ??
  if(!Rf_isInteger(node_type))
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid type", cg_get_name(node));
  }

  return INTEGER(node_type)[0];
}

void cg_set_type(SEXP node, int type)
{
  if(type < 0 || type > 3)
  {
    Rf_errorcall(R_NilValue, "invalid type provided");
  }

  Rf_setAttrib(node, CG_TypeSymbol, Rf_ScalarInteger(type));
}

SEXP cg_get_call(SEXP node)
{
  SEXP node_call = Rf_getAttrib(node, CG_CallSymbol);

  if(!(Rf_isSymbol(node_call) || Rf_isLanguage(node_call)))
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid call", cg_get_name(node));
  }

  return node_call;
}

void cg_set_call(SEXP node, SEXP call)
{
  if(!(Rf_isSymbol(call) || Rf_isLanguage(call)))
  {
    Rf_errorcall(R_NilValue, "call must be a symbol or call");
  }

  Rf_setAttrib(node, CG_CallSymbol, call);
}

SEXP cg_get_grads(SEXP node)
{
  SEXP node_grads = Rf_getAttrib(node, CG_GradsSymbol);

  if(TYPEOF(node_grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid gradients", cg_get_name(node));
  }

  return node_grads;
}

void cg_set_grads(SEXP node, SEXP grads)
{
  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "grads must be a list");
  }

  Rf_setAttrib(node, CG_GradsSymbol, grads);
}

SEXP cg_get_grad(SEXP node, int index)
{
  SEXP node_grads = Rf_getAttrib(node, CG_GradsSymbol);

  if(TYPEOF(node_grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid gradients", cg_get_name(node));
  }

  int n = LENGTH(node_grads);

  if(index < 0 || index > n - 1)
  {
    Rf_errorcall(R_NilValue, "cannot retrieve gradient of node '%s' at index %d", cg_get_name(node), index);
  }

  return VECTOR_ELT(node_grads, index);
}

void cg_add_grad(SEXP node, SEXP grad)
{
  int node_grads_index;

  SEXP node_grads = R_NilValue;

  if(!(Rf_isSymbol(grad) || Rf_isLanguage(grad)))
  {
    Rf_errorcall(R_NilValue, "grad must be a symbol or call");
  }

  PROTECT_WITH_INDEX(node_grads = Rf_getAttrib(node, CG_GradsSymbol), &node_grads_index);

  if(TYPEOF(node_grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid gradients", cg_get_name(node));
  }

  int n = LENGTH(node_grads);

  REPROTECT(node_grads = Rf_lengthgets(node_grads, n + 1), node_grads_index);

  SET_VECTOR_ELT(node_grads, n, grad);

  Rf_setAttrib(node, CG_GradsSymbol, node_grads);

  UNPROTECT(1);
}

SEXP cg_get_parents(SEXP node)
{
  SEXP node_parents = Rf_getAttrib(node, CG_ParentsSymbol);

  if(TYPEOF(node_parents) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid parents", cg_get_name(node));
  }

  return node_parents;
}

void cg_set_parents(SEXP node, SEXP parents)
{
  if(TYPEOF(parents) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "parents must be a list");
  }

  Rf_setAttrib(node, CG_ParentsSymbol, parents);
}

SEXP cg_get_parent(SEXP node, int index)
{
  SEXP node_parents = Rf_getAttrib(node, CG_ParentsSymbol);

  if(TYPEOF(node_parents) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid parents", cg_get_name(node));
  }

  int n = LENGTH(node_parents);

  if(index < 0 || index > n - 1)
  {
    Rf_errorcall(R_NilValue, "cannot retrieve parent of node '%s' at index %d", cg_get_name(node), index);
  }

  return VECTOR_ELT(node_parents, index);
}

void cg_add_parent(SEXP node, SEXP parent)
{
  int node_parents_index;

  SEXP node_parents = R_NilValue;

  if(!Rf_isSymbol(parent))
  {
    Rf_errorcall(R_NilValue, "parent must be a symbol");
  }

  PROTECT_WITH_INDEX(node_parents = Rf_getAttrib(node, CG_ParentsSymbol), &node_parents_index);

  if(TYPEOF(node_parents) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid parents", cg_get_name(node));
  }

  int n = LENGTH(node_parents);

  REPROTECT(node_parents = Rf_lengthgets(node_parents, n + 1), node_parents_index);

  SET_VECTOR_ELT(node_parents, n, parent);

  Rf_setAttrib(node, CG_ParentsSymbol, node_parents);

  UNPROTECT(1);
}

SEXP cg_get_childeren(SEXP node)
{
  SEXP node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol);

  if(TYPEOF(node_childeren) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid childeren", cg_get_name(node));
  }

  return node_childeren;
}

void cg_set_childeren(SEXP node, SEXP childeren)
{
  if(TYPEOF(childeren) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "childeren must be a list");
  }

  Rf_setAttrib(node, CG_ChilderenSymbol, childeren);
}

SEXP cg_get_child(SEXP node, int index)
{
  SEXP node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol);

  if(TYPEOF(node_childeren) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid childeren", cg_get_name(node));
  }

  int n = LENGTH(node_childeren);

  if(index < 0 || index > n - 1)
  {
    Rf_errorcall(R_NilValue, "cannot retrieve child of node '%s' at index %d", cg_get_name(node), index);
  }

  return VECTOR_ELT(node_childeren, index);
}

void cg_add_child(SEXP node, SEXP child)
{
  int node_childeren_index;

  SEXP node_childeren = R_NilValue;

  if(!Rf_isSymbol(child))
  {
    Rf_errorcall(R_NilValue, "child must be a symbol");
  }

  PROTECT_WITH_INDEX(node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol), &node_childeren_index);

  if(TYPEOF(node_childeren) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has invalid childeren", cg_get_name(node));
  }

  int n = LENGTH(node_childeren);

  REPROTECT(node_childeren = Rf_lengthgets(node_childeren, n + 1), node_childeren_index);

  SET_VECTOR_ELT(node_childeren, n, child);

  Rf_setAttrib(node, CG_ChilderenSymbol, node_childeren);

  UNPROTECT(1);
}

int cg_node_exists(SEXP symbol, SEXP graph)
{
  int exists = FALSE;

  SEXP nodes = PROTECT(cg_get_nodes(graph));

  if(!Rf_isSymbol(symbol))
  {
    Rf_errorcall(R_NilValue, "symbol must be a symbol");
  }

  if(Rf_findVarInFrame(nodes, symbol) != R_UnboundValue)
  {
    exists = TRUE;
  }

  UNPROTECT(1);

  return exists;
}

SEXP cg_get_node(SEXP symbol, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  if(!Rf_isSymbol(symbol))
  {
    Rf_errorcall(R_NilValue, "symbol must be a symbol");
  }

  SEXP node = Rf_findVarInFrame(nodes, symbol);

  if(node == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "cannot find node '%s'", CHAR(Rf_asChar(symbol)));
  }

  UNPROTECT(1);

  return node;
}

void cg_set_node(SEXP node, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  if(!is_cg_node(node))
  {
    Rf_errorcall(R_NilValue, "invalid cg.node object provided");
  }

  SEXP node_symbol = cg_get_symbol(node);

  if(!cg_node_exists(node_symbol, graph))
  {
    Rf_errorcall(R_NilValue, "cannot find node '%s'", cg_get_name(node));
  }

  Rf_setVar(node_symbol, node, nodes);

  UNPROTECT(1);
}

void cg_add_node(SEXP node, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  SEXP node_symbol = cg_get_symbol(node);

  if(cg_node_exists(node_symbol, graph))
  {
    Rf_errorcall(R_NilValue, "'%s' is already defined", cg_get_name(node));
  }

  Rf_defineVar(node_symbol, node, nodes);

  UNPROTECT(1);
}

static void cg_add_value(SEXP node, SEXP value, SEXP graph)
{
  SEXP values = PROTECT(cg_get_values(graph));

  if(!is_cg_node(node))
  {
    Rf_errorcall(R_NilValue, "invalid cg.node object provided");
  }

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "node '%s' does not evaluate to a numeric vector or array but a '%s'",
                  CHAR(Rf_asChar(node)),  Rf_type2char(TYPEOF(value)));
  }

  if(!Rf_isReal(value))
  {
    value = PROTECT(Rf_coerceVector(value, REALSXP));

    Rf_defineVar(Rf_install(CHAR(Rf_asChar(node))), value, values);

    UNPROTECT(1);
  }
  else
  {
    Rf_defineVar(Rf_install(CHAR(Rf_asChar(node))), value, values);
  }

  UNPROTECT(1);
}

SEXP cg_gen_name(SEXP graph)
{
  char name[32];

  SEXP nodes = PROTECT(cg_get_nodes(graph));

  sprintf(name, "node%d", (int) Rf_xlength(nodes) + 1);

  UNPROTECT(1);

  return Rf_mkString(name);
}

int cg_node_id(SEXP name, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  if(!(Rf_isString(name) || Rf_isSymbol(name)))
  {
    Rf_errorcall(R_NilValue, "name must be a character scalar or symbol");
  }

  for(int i = LENGTH(nodes) - 1; i >= 0; i--)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(CHAR(Rf_asChar(node)), CHAR(Rf_asChar(name))) == 0)
    {
      UNPROTECT(1);

      return i + 1;
    }
  }

  Rf_errorcall(R_NilValue, "cannot find node '%s'", CHAR(Rf_asChar(name)));
}

SEXP cg_node(SEXP type, SEXP name)
{
  if(!Rf_isNumber(type))
  {
    Rf_errorcall(R_NilValue, "type must be an numeric scalar");
  }

  if(!Rf_isString(name))
  {
    Rf_errorcall(R_NilValue, "the name of a node must be a character scalar");
  }

  int node_type = Rf_asInteger(type);

  SEXP node = PROTECT(Rf_allocVector(STRSXP, 1));

  cg_set_name(node, CHAR(Rf_asChar(name)));

  cg_set_type(node, node_type);

  switch(node_type)
  {
    case CGCST :
    case CGIPT :
    case CGPRM :
    {
      cg_set_grads(node, Rf_allocVector(VECSXP, 0));
      cg_set_childeren(node, Rf_allocVector(VECSXP, 0));

      break;
    }
    case CGOPR :
    {
      cg_set_call(node, Rf_allocVector(LANGSXP, 1));
      cg_set_grads(node, Rf_allocVector(VECSXP, 0));
      cg_set_parents(node, Rf_allocVector(VECSXP, 0));
      cg_set_childeren(node, Rf_allocVector(VECSXP, 0));

      break;
    }
  }

  Rf_setAttrib(node, R_ClassSymbol, Rf_mkString("cg.node"));

  UNPROTECT(1);

  return node;
}

SEXP cg_add_constant(SEXP value, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  SEXP node_type = PROTECT(Rf_ScalarInteger(CGCST));

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(node_type, node_name));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(3);

  return node;
}

SEXP cg_add_input(SEXP value, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  SEXP node_type = PROTECT(Rf_ScalarInteger(CGIPT));

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(node_type, node_name));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(3);

  return node;
}

SEXP cg_add_parameter(SEXP value, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  SEXP node_type = PROTECT(Rf_ScalarInteger(CGPRM));

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(node_type, node_name));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(3);

  return node;
}

SEXP cg_get_parms(SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  SEXP values = PROTECT(cg_get_values(graph));

  int n = LENGTH(nodes), l = 0;

  SEXP parms = PROTECT(Rf_allocVector(VECSXP, n));

  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(!is_cg_node(node))
    {
      Rf_errorcall(R_NilValue, "node '%s' is not a valid cg.node object", CHAR(Rf_asChar(node)));
    }

    SEXP node_type = Rf_getAttrib(node, Rf_install("type"));

    if(!Rf_isInteger(node_type))
    {
      Rf_errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(Rf_asChar(node)));
    }

    if(Rf_asInteger(node_type) == CGPRM)
    {
      SEXP value = PROTECT(Rf_findVarInFrame(values, Rf_install(CHAR(Rf_asChar(node)))));

      if(value != R_UnboundValue)
      {
        SET_VECTOR_ELT(parms, l, value);
      }

      SET_STRING_ELT(names, l, Rf_asChar(node));

      UNPROTECT(1);

      l++;
    }
  }

  SETLENGTH(parms, l);
  SETLENGTH(names, l);

  Rf_setAttrib(parms, R_NamesSymbol, names);

  UNPROTECT(4);

  return(parms);
}

SEXP cg_add_parms(SEXP parms, SEXP graph)
{
  SEXP names = Rf_getAttrib(parms, R_NamesSymbol);

  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "parms must be a named list");
  }

  for(int i = 0; i < LENGTH(parms); i++)
  {
    SEXP value = VECTOR_ELT(parms, i);

    if(Rf_isNull(names))
    {
      cg_add_parameter(value, R_NilValue, graph);
    }
    else
    {
      SEXP name = PROTECT(Rf_ScalarString(STRING_ELT(names, i)));

      cg_add_parameter(value, name, graph);

      UNPROTECT(1);
    }
  }

  return R_NilValue;
}

SEXP cg_binding(SEXP x)
{
  SEXP binding = PROTECT(NewEnv(R_EmptyEnv));

  SEXP names = Rf_getAttrib(x, R_NamesSymbol);

  if(TYPEOF(x) != VECSXP || !Rf_isString(names))
  {
    Rf_errorcall(R_NilValue, "x must be a named list");
  }

  for(int i = 0; i < LENGTH(x); i++)
  {
    SEXP var = STRING_ELT(names, i);

    if(Rf_asChar(var) == R_BlankString)
    {
      Rf_errorcall(R_NilValue, "one of the members of x is not properly named");
    }

    SEXP node_name = VECTOR_ELT(x, i);

    if(!Rf_isString(node_name) || Rf_asChar(node_name) == R_BlankString)
    {
      Rf_errorcall(R_NilValue, "the node bound to '%s' has an invalid name", CHAR(var));
    }

    Rf_defineVar(Rf_install(CHAR(var)), Rf_install(CHAR(Rf_asChar(node_name))), binding);

    UNPROTECT(1);
  }

  return binding;
}

SEXP cg_add_operation(SEXP call, SEXP grads, SEXP binding, SEXP name, SEXP graph)
{
  SEXP node_type = PROTECT(Rf_ScalarInteger(CGOPR));

  SEXP node_name = R_NilValue;

  if(!(Rf_isSymbol(call) || Rf_isLanguage(call)))
  {
    Rf_errorcall(R_NilValue, "call must be a symbol or call");
  }

  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "grads must be a named list");
  }

  if(!Rf_isEnvironment(binding))
  {
    Rf_errorcall(R_NilValue, "binding must be an environment");
  }

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(node_type, node_name));

  SEXP grad_names = Rf_getAttrib(grads, R_NamesSymbol);

  if(Rf_isString(grad_names))
  {
    int n = LENGTH(grad_names);

    SEXP node_parents = PROTECT(Rf_allocVector(VECSXP, n));

    for(int i = 0; i < n; i++)
    {
      SEXP grad_name = STRING_ELT(grad_names, i);

      if(grad_name == R_BlankString)
      {
        Rf_errorcall(R_NilValue, "blank name provided for gradient at index %d", i + 1);
      }

      SEXP parent_name = PROTECT(Rf_findVarInFrame(binding, Rf_install(CHAR(grad_name))));

      if(parent_name == R_UnboundValue || !Rf_isSymbol(parent_name))
      {
        Rf_errorcall(R_NilValue, "cannot find symbol '%s' in binding", CHAR(STRING_ELT(grad_names, i)));
      }

      SET_VECTOR_ELT(node_parents, i, parent_name);

      UNPROTECT(1);
    }

    for(int i = 0; i < n; i++)
    {
      SEXP parent = PROTECT(cg_get_node(VECTOR_ELT(node_parents, i), graph));

      cg_add_grad(parent, Rf_substitute(VECTOR_ELT(grads, i), binding));

      cg_add_child(parent, Rf_install(cg_get_name(node)));

      cg_set_node(parent, graph);

      UNPROTECT(1);
    }

    cg_set_parents(node, node_parents);

    UNPROTECT(1);
  }

  cg_set_call(node, Rf_substitute(call, binding));

  cg_add_node(node, graph);

  UNPROTECT(3);

return node;
}

static SEXP cg_traverse_graph(SEXP name, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int l = 0, n = LENGTH(nodes), visited[n];

  SEXP ids = PROTECT(Rf_allocVector(INTSXP, n));

  memset(visited, 0, n * sizeof(int));

  stack *s = stack_allocate(n);

  stack_add(s, cg_node_id(name, graph));

  while(!stack_is_empty(s))
  {
    int current = stack_current(s);

    SEXP node = VECTOR_ELT(nodes, current - 1);

    SEXP parents = Rf_getAttrib(node, Rf_install("parents"));

    if(visited[current - 1] == 0)
    {
      if(Rf_isInteger(parents))
      {
        int m = LENGTH(parents);

        for(int i = 0; i < m; i++)
        {
          if(visited[INTEGER(parents)[i] - 1] == 0)
          {
            stack_add(s, INTEGER(parents)[i]);
          }
        }
      }
      else
      {
        INTEGER(ids)[l] = stack_get(s);

        l++;
      }
    }
    else
    {
      if(visited[current - 1] == 1)
      {
        if(Rf_isInteger(parents))
        {
          INTEGER(ids)[l] = stack_get(s);

          l++;
        }
        else
        {
          stack_remove(s);
        }
      }
      else
      {
        stack_remove(s);
      }
    }

    visited[current - 1] += 1;
  }

  SETLENGTH(ids, l);

  stack_destroy(s);

  UNPROTECT(2);

  return ids;
}

static void cg_forward(SEXP ids, SEXP values, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  for(int i = 0; i < LENGTH(ids); i++)
  {
    SEXP node = VECTOR_ELT(nodes, INTEGER(ids)[i] - 1);

    if(!is_cg_node(node))
    {
      Rf_errorcall(R_NilValue, "node '%s' is not a valid cg.node object", CHAR(Rf_asChar(node)));
    }

    SEXP node_type = Rf_getAttrib(node, Rf_install("type"));

    if(!Rf_isInteger(node_type))
    {
      Rf_errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(Rf_asChar(node)));
    }

    SEXP value = R_NilValue;

    if(Rf_asInteger(node_type) == CGOPR)
    {
      SEXP call = Rf_getAttrib(node, Rf_install("call"));

      if(!(Rf_isLanguage(call) || Rf_isSymbol(call)))
      {
        Rf_errorcall(R_NilValue, "node '%s' has an invalid call", CHAR(Rf_asChar(node)));
      }

      value = PROTECT(Rf_eval(call, values));

      Rf_defineVar(Rf_install(CHAR(Rf_asChar(node))), value, values);
    }
    else
    {
      value = PROTECT(Rf_eval(Rf_install(CHAR(Rf_asChar(node))), values));
    }

    if(!Rf_isNumeric(value))
    {
      Rf_errorcall(R_NilValue, "object '%s' does not evaluate to a numeric vector or array but a '%s'",
                CHAR(Rf_asChar(node)), Rf_type2char(TYPEOF(value)));
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
}

static void cg_backward(SEXP ids, SEXP index, SEXP values, SEXP grads, SEXP graph)
{
  int n = LENGTH(ids);

  SEXP nodes = PROTECT(cg_get_nodes(graph));

  SEXP child_grad_env = PROTECT(NewEnv(values));

  if(n > 0)
  {
    SEXP root = VECTOR_ELT(nodes, INTEGER(ids)[n - 1] - 1);

    if(!is_cg_node(root))
    {
      Rf_errorcall(R_NilValue, "node '%s' is not a valid cg.node object", CHAR(Rf_asChar(root)));
    }

    PROTECT_INDEX ipx;

    SEXP root_grad = R_NilValue;

    PROTECT_WITH_INDEX(root_grad = Rf_eval(Rf_install(CHAR(Rf_asChar(root))), values), &ipx);

    REPROTECT(root_grad = Rf_duplicate(root_grad), ipx);

    if(!Rf_isNumber(root_grad))
    {
      Rf_errorcall(R_NilValue, "cannot differentiate an object of type '%s'", Rf_type2char(TYPEOF(root_grad)));
    }

    if(!Rf_isReal(root_grad))
    {
      REPROTECT(root_grad = Rf_coerceVector(root_grad, REALSXP), ipx);
    }

    int m = LENGTH(root_grad);

    if(Rf_asInteger(index) < 1 || Rf_asInteger(index) > m)
    {
      Rf_errorcall(R_NilValue, "invalid index provided");
    }

    memset(REAL(root_grad), 0, m * sizeof(double));

    REAL(root_grad)[Rf_asInteger(index) - 1] = 1;

    Rf_defineVar(Rf_install(CHAR(Rf_asChar(root))), root_grad, grads);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = VECTOR_ELT(nodes, INTEGER(ids)[i] - 1);

      if(!is_cg_node(node))
      {
        Rf_errorcall(R_NilValue, "node '%s' is not a valid cg.node object", CHAR(Rf_asChar(node)));
      }

      SEXP node_type = Rf_getAttrib(node, Rf_install("type"));

      if(!Rf_isInteger(node_type))
      {
        Rf_errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(Rf_asChar(node)));
      }

      if(Rf_asInteger(node_type) == CGPRM || Rf_asInteger(node_type) == CGOPR)
      {
        SEXP node_grads = Rf_getAttrib(node, Rf_install("grads"));

        SEXP node_childeren = Rf_getAttrib(node, Rf_install("childeren"));

        if(TYPEOF(node_grads) != VECSXP)
        {
          Rf_errorcall(R_NilValue, "node '%s' has invalid gradients", CHAR(Rf_asChar(node)));
        }

        if(!Rf_isInteger(node_childeren))
        {
          Rf_errorcall(R_NilValue, "node '%s' has invalid childeren", CHAR(Rf_asChar(node)));
        }

        if(LENGTH(node_grads) != LENGTH(node_childeren))
        {
          Rf_errorcall(R_NilValue, "node '%s' has an unequal number of gradients (%d) and childeren (%d)",
                        CHAR(Rf_asChar(node)), LENGTH(node_grads), LENGTH(node_childeren));
        }

        SEXP node_grad = R_NilValue;

        for(int j = 0; j < LENGTH(node_childeren); j++)
        {
          SEXP child = VECTOR_ELT(nodes, INTEGER(node_childeren)[j] - 1);

          SEXP child_grad = PROTECT(Rf_findVarInFrame(grads, Rf_install(CHAR(Rf_asChar(child)))));

          if(child_grad != R_UnboundValue)
          {
            Rf_defineVar(Rf_install("grad"), child_grad, child_grad_env);

            SEXP node_grad_call = VECTOR_ELT(node_grads, j);

            if(!(Rf_isLanguage(node_grad_call) || Rf_isSymbol(node_grad_call)))
            {
              Rf_errorcall(R_NilValue, "node '%s' has an invalid gradient at index %d", CHAR(Rf_asChar(node)), j + 1);
            }

            PROTECT_INDEX ipy;

            SEXP current_grad = R_NilValue;

            PROTECT_WITH_INDEX(current_grad = Rf_eval(node_grad_call, child_grad_env), &ipy);

            if(!Rf_isNumeric(current_grad))
            {
              Rf_errorcall(R_NilValue, "the gradient of node '%s' at index %d does not evaluate to a numeric vector or array but a '%s'",
                            CHAR(Rf_asChar(node)), j + 1, Rf_type2char(TYPEOF(current_grad)));
            }

            if(!Rf_isReal(current_grad))
            {
              REPROTECT(current_grad = Rf_coerceVector(current_grad, REALSXP), ipy);
            }

            if(Rf_isNull(node_grad))
            {
              if(Rf_isSymbol(node_grad_call))
              {
                node_grad = PROTECT(Rf_duplicate(current_grad));
              }
              else
              {
                node_grad = PROTECT(current_grad);
              }
            }
            else
            {
              for(int k = 0; k < LENGTH(current_grad); k++)
              {
                REAL(node_grad)[k] += REAL(current_grad)[k];
              }
            }

            UNPROTECT_PTR(current_grad);
          }

          UNPROTECT_PTR(child_grad);
        }

        Rf_defineVar(Rf_install(CHAR(Rf_asChar(node))), node_grad, grads);

        if(!Rf_isNull(node_grad))
        {
          UNPROTECT_PTR(node_grad);
        }
      }
    }

    UNPROTECT(1);
  }

  UNPROTECT(2);
}

SEXP cg_run(SEXP name, SEXP values, SEXP graph)
{
  SEXP graph_values = PROTECT(cg_get_values(graph));

  if(!Rf_isString(name) || Rf_asChar(name) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "name must be a non-blank character scalar");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  SEXP enclos = PROTECT(ENCLOS(values));

  SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  if(values != graph_values)
  {
    SET_ENCLOS(values, graph_values);
  }

  cg_forward(ids, values, graph);

  SET_ENCLOS(values, enclos);

  UNPROTECT(3);

  return values;
}

SEXP cg_gradients(SEXP name, SEXP values, SEXP index, SEXP graph)
{
  SEXP grads = PROTECT(NewEnv(R_NilValue));

  SEXP graph_values = PROTECT(cg_get_values(graph));

  if(!Rf_isString(name) || Rf_asChar(name) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "name must be a non-blank character scalar");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isNumber(index))
  {
    Rf_errorcall(R_NilValue, "index must be a numeric scalar");
  }

  SEXP enclos = PROTECT(ENCLOS(values));

  SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  if(values != graph_values)
  {
    SET_ENCLOS(values, graph_values);
  }

  cg_backward(ids, index, values, grads, graph);

  SET_ENCLOS(values, enclos);

  UNPROTECT(4);

  return grads;
}

SEXP cg_approx_grad(SEXP x, SEXP y, SEXP values, SEXP index, SEXP eps, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  SEXP graph_values = PROTECT(cg_get_values(graph));

  if(!Rf_isString(x) || Rf_asChar(x) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "x must be a non-blank character scalar");
  }

  if(!Rf_isString(y) || Rf_asChar(y) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "y must be a non-blank character scalar");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isNumber(index))
  {
    Rf_errorcall(R_NilValue, "index must be a numeric scalar");
  }

  if(!Rf_isNumber(eps))
  {
    Rf_errorcall(R_NilValue, "eps must be a numeric scalar");
  }

  SEXP enclos = PROTECT(ENCLOS(values));

  SEXP ids = PROTECT(cg_traverse_graph(x, graph));

  SEXP x_node = VECTOR_ELT(nodes, cg_node_id(x, graph) - 1);
  SEXP y_node = VECTOR_ELT(nodes, cg_node_id(y, graph) - 1);

  if(values != graph_values)
  {
    SET_ENCLOS(values, graph_values);
  }

  cg_forward(ids, values, graph);

  PROTECT_INDEX ipx;
  PROTECT_INDEX ipy;

  SEXP x_value = R_NilValue;
  SEXP y_value = R_NilValue;

  PROTECT_WITH_INDEX(x_value = Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values), &ipx);
  PROTECT_WITH_INDEX(y_value = Rf_eval(Rf_install(CHAR(Rf_asChar(y_node))), values), &ipy);

  if(!Rf_isNumeric(x_value))
  {
    Rf_errorcall(R_NilValue, "object '%s' does not evaluate to a numeric vector or array but a '%s'",
                 CHAR(Rf_asChar(x_node)), Rf_type2char(TYPEOF(x_value)));
  }

  if(!Rf_isNumeric(y_value))
  {
    Rf_errorcall(R_NilValue, "object '%s' does not evaluate to a numeric vector or array but a '%s'",
                 CHAR(Rf_asChar(y_node)), Rf_type2char(TYPEOF(y_value)));
  }

  if(!Rf_isReal(y_value))
  {
    REPROTECT(y_value = Rf_coerceVector(y_value, REALSXP), ipy);
  }

  if(Rf_asInteger(index) < 1 || Rf_asInteger(index) > LENGTH(x_value))
  {
    Rf_errorcall(R_NilValue, "invalid index provided");
  }

  SEXP grad = PROTECT(Rf_duplicate(y_value));

  for(int i = 0; i < LENGTH(grad); i++)
  {
    PROTECT_INDEX ipx1;
    PROTECT_INDEX ipx2;

    SEXP x_value1 = R_NilValue;
    SEXP x_value2 = R_NilValue;

    REAL(y_value)[i] += Rf_asReal(eps);

    cg_forward(ids, values, graph);

    PROTECT_WITH_INDEX(x_value1 = Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values), &ipx1);

    if(!Rf_isReal(x_value1))
    {
      REPROTECT(x_value1 = Rf_coerceVector(x_value1, REALSXP), ipx1);
    }

    REAL(y_value)[i] -= 2 * Rf_asReal(eps);

    cg_forward(ids, values, graph);

    PROTECT_WITH_INDEX(x_value2 = Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values), &ipx2);

    if(!Rf_isReal(x_value2))
    {
      REPROTECT(x_value2 = Rf_coerceVector(x_value2, REALSXP), ipx2);
    }

    REAL(grad)[i] = (REAL(x_value1)[Rf_asInteger(index) - 1] - REAL(x_value2)[Rf_asInteger(index) - 1]) / (2 * Rf_asReal(eps));

    REAL(y_value)[i] += Rf_asReal(eps);

    UNPROTECT(2);
  }

  SET_ENCLOS(values, enclos);

  UNPROTECT(7);

  return grad;
}

SEXP cg_adj_mat(SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = LENGTH(nodes);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));

  SEXP mat = PROTECT(Rf_allocMatrix(INTSXP, n, n));

  memset(INTEGER(mat), 0, n * n * sizeof(int));

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    SEXP childeren = Rf_getAttrib(node, Rf_install("childeren"));

    if(!Rf_isInteger(childeren))
    {
      Rf_errorcall(R_NilValue, "node '%s' has invalid childeren", CHAR(Rf_asChar(node)));
    }

    for(int j = 0; j < LENGTH(childeren); j++)
    {
      INTEGER(mat)[i + n * (INTEGER(childeren)[j] - 1)] = 1;
    }

    SET_STRING_ELT(names, i, Rf_asChar(node));
  }

  SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(dimnames, 0, names);
  SET_VECTOR_ELT(dimnames, 1, names);

  Rf_setAttrib(mat, R_DimNamesSymbol, dimnames);

  UNPROTECT(4);

  return mat;
}
