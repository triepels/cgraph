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

SEXP cgraph(SEXP graph, SEXP values)
{
  int n = 0;

  SEXP nodes = PROTECT(Rf_allocVector(VECSXP, 0));

  if(Rf_isNull(graph) || !Rf_isEnvironment(graph))
  {
    graph = PROTECT(NewEnv(R_EmptyEnv));

    n++;
  }

  if(Rf_isNull(values) || !Rf_isEnvironment(values))
  {
    values = PROTECT(NewEnv(R_BaseEnv));

    n++;
  }

  Rf_setAttrib(nodes, R_NamesSymbol, Rf_allocVector(STRSXP, 0));

  Rf_defineVar(CG_NodesSymbol, nodes, graph);
  Rf_defineVar(CG_ValuesSymbol, values, graph);

  Rf_setAttrib(graph, R_ClassSymbol, Rf_mkString("cgraph"));

  UNPROTECT(n + 1);

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

SEXP cg_get_nodes(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    Rf_errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, CG_NodesSymbol));

  if(TYPEOF(nodes) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "cannot find nodes in cgraph object");
  }

  UNPROTECT(1);

  return nodes;
}

SEXP cg_get_values(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    Rf_errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP values = PROTECT(Rf_findVarInFrame(graph, CG_ValuesSymbol));

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "cannot find values in cgraph object");
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

  SET_STRING_ELT(node, 0, Rf_mkChar(name));
}

SEXP cg_get_symbol(SEXP node)
{
  return Rf_install(cg_get_name(node));
}

int cg_get_type(SEXP node)
{
  SEXP node_type = Rf_getAttrib(node, CG_TypeSymbol);
  // SIZE = 0 ??
  if(!Rf_isInteger(node_type))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no type", cg_get_name(node));
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

  if(!Rf_isSymbol(node_call))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no call", cg_get_name(node));
  }

  return node_call;
}

void cg_set_call(SEXP node, SEXP call)
{
  if(!Rf_isSymbol(call))
  {
    Rf_errorcall(R_NilValue, "call must be a symbol");
  }

  Rf_setAttrib(node, CG_CallSymbol, call);
}

SEXP cg_get_grads(SEXP node)
{
  SEXP node_grads = Rf_getAttrib(node, CG_GradsSymbol);

  if(TYPEOF(node_grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no gradients", cg_get_name(node));
  }

  for(int i = 0; i < LENGTH(node_grads); i++)
  {
    SEXP grad = VECTOR_ELT(node_grads, i);

    if(!Rf_isSymbol(grad))
    {
      Rf_errorcall(R_NilValue, "node '%s' has an invalid gradient at index %d", cg_get_name(node), i);
    }
  }

  return node_grads;
}

void cg_set_grads(SEXP node, SEXP grads)
{
  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "grads must be a list");
  }

  for(int i = 0; i < LENGTH(grads); i++)
  {
    SEXP grad = VECTOR_ELT(grads, i);

    if(!Rf_isSymbol(grad))
    {
      Rf_errorcall(R_NilValue, "invalid gradient provided at index %d", i);
    }
  }

  Rf_setAttrib(node, CG_GradsSymbol, grads);
}

SEXP cg_get_grad(SEXP node, int index)
{
  SEXP node_grads = Rf_getAttrib(node, CG_GradsSymbol);

  if(TYPEOF(node_grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "node '%s' has no gradients", cg_get_name(node));
  }

  if(index < 0 || index > LENGTH(node_grads) - 1)
  {
    Rf_errorcall(R_NilValue, "cannot retrieve gradient of node '%s' at index %d", cg_get_name(node), index);
  }

  SEXP grad = VECTOR_ELT(node_grads, index);

  if(!Rf_isSymbol(grad))
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid gradient at index %d", cg_get_name(node), index);
  }

  return grad;
}

int cg_add_grad(SEXP node, SEXP grad)
{
  int node_grads_index;

  SEXP node_grads = R_NilValue;

  if(!Rf_isSymbol(grad))
  {
    Rf_errorcall(R_NilValue, "grad must be a symbol");
  }

  PROTECT_WITH_INDEX(node_grads = Rf_getAttrib(node, CG_GradsSymbol), &node_grads_index);

  int index = 0;

  if(TYPEOF(node_grads) != VECSXP)
  {
    REPROTECT(node_grads = Rf_allocVector(VECSXP, 1), node_grads_index);

    SET_VECTOR_ELT(node_grads, index, grad);
  }
  else
  {
    index = LENGTH(node_grads);

    REPROTECT(node_grads = Rf_lengthgets(node_grads, index + 1), node_grads_index);

    SET_VECTOR_ELT(node_grads, index, grad);
  }

  Rf_setAttrib(node, CG_GradsSymbol, node_grads);

  UNPROTECT(1);

  return index;
}

int cg_has_parents(SEXP node)
{
  int has_parents = FALSE;

  SEXP node_parents = Rf_getAttrib(node, CG_ParentsSymbol);

  if(Rf_isInteger(node_parents))
  {
    if(LENGTH(node_parents) > 0)
    {
      has_parents = TRUE;
    }
  }

  return has_parents;
}

int* cg_get_parents(SEXP node, int* length)
{
  SEXP node_parents = Rf_getAttrib(node, CG_ParentsSymbol);

  if(!Rf_isInteger(node_parents))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no parents", cg_get_name(node));
  }

  (*length) = LENGTH(node_parents);

  return INTEGER(node_parents);
}

int cg_add_parent(SEXP node, int id)
{
  int node_parents_index;

  SEXP node_parents = R_NilValue;

  PROTECT_WITH_INDEX(node_parents = Rf_getAttrib(node, CG_ParentsSymbol), &node_parents_index);

  int index = 0;

  if(!Rf_isInteger(node_parents))
  {
    REPROTECT(node_parents = Rf_ScalarInteger(id), node_parents_index);
  }
  else
  {
    index = LENGTH(node_parents);

    REPROTECT(node_parents = Rf_lengthgets(node_parents, index + 1), node_parents_index);

    INTEGER(node_parents)[index] = id;
  }

  Rf_setAttrib(node, CG_ParentsSymbol, node_parents);

  UNPROTECT(1);

  return index;
}

int cg_has_childeren(SEXP node)
{
  int has_childeren = FALSE;

  SEXP node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol);

  if(Rf_isInteger(node_childeren))
  {
    if(LENGTH(node_childeren) > 0)
    {
      has_childeren = TRUE;
    }
  }

  return has_childeren;
}

int* cg_get_childeren(SEXP node, int* length)
{
  SEXP node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol);

  if(!Rf_isInteger(node_childeren))
  {
    Rf_errorcall(R_NilValue, "node '%s' has no childeren", cg_get_name(node));
  }

  (*length) = LENGTH(node_childeren);

  return INTEGER(node_childeren);
}

int cg_add_child(SEXP node, int id)
{
  int node_childeren_index;

  SEXP node_childeren = R_NilValue;

  PROTECT_WITH_INDEX(node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol), &node_childeren_index);

  int index = 0;

  if(!Rf_isInteger(node_childeren))
  {
    REPROTECT(node_childeren = Rf_ScalarInteger(id), node_childeren_index);
  }
  else
  {
    index = LENGTH(node_childeren);

    REPROTECT(node_childeren = Rf_lengthgets(node_childeren, index + 1), node_childeren_index);

    INTEGER(node_childeren)[index] = id;
  }

  Rf_setAttrib(node, CG_ChilderenSymbol, node_childeren);

  UNPROTECT(1);

  return index;
}

SEXP cg_gen_name(SEXP graph)
{
  char name[32];

  SEXP nodes = PROTECT(cg_get_nodes(graph));

  sprintf(name, "node%d", (int) Rf_xlength(nodes) + 1);

  UNPROTECT(1);

  return Rf_mkString(name);
}

SEXP cg_node(int type, const char* name)
{
  SEXP node = PROTECT(Rf_allocVector(STRSXP, 1));

  cg_set_type(node, type);

  cg_set_name(node, name);

  Rf_setAttrib(node, R_ClassSymbol, Rf_mkString("cg.node"));

  UNPROTECT(1);

  return node;
}

int cg_node_id(SEXP node, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  const char* node_name = cg_get_name(node);

  for(int i = LENGTH(nodes) - 1; i >= 0; i--)
  {
    if(strcmp(cg_get_name(VECTOR_ELT(nodes, i)), node_name) == 0)
    {
      UNPROTECT(1);

      return i + 1;
    }
  }

  Rf_errorcall(R_NilValue, "cannot find node '%s'", node_name);
}

int cg_node_exists(const char* name, SEXP graph)
{
  int exists = FALSE;

  SEXP nodes = PROTECT(cg_get_nodes(graph));

  for(int i = 0; i < LENGTH(nodes); i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(cg_get_name(node), name) == 0)
    {
      exists = TRUE;

      break;
    }
  }

  UNPROTECT(1);

  return exists;
}

SEXP cg_get_node(const char* name, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  for(int i = 0; i < LENGTH(nodes); i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(cg_get_name(node), name) == 0)
    {
      UNPROTECT(1);

      return node;
    }
  }

  Rf_errorcall(R_NilValue, "cannot find node '%s'", name);
}

SEXP cg_get_node_id(int id, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  if(id < 1 || id > LENGTH(nodes))
  {
    Rf_errorcall(R_NilValue, "cannot find node with id %d", id);
  }

  SEXP node = VECTOR_ELT(nodes, id - 1);

  UNPROTECT(1);

  return node;
}

int cg_add_node(SEXP node, SEXP graph)
{
  const char* node_name = cg_get_name(node);

  if(cg_node_exists(node_name, graph))
  {
    Rf_errorcall(R_NilValue, "'%s' is already defined in the graph", node_name);
  }

  int nodes_index, names_index;

  SEXP nodes = R_NilValue, names = R_NilValue;

  PROTECT_WITH_INDEX(nodes = cg_get_nodes(graph), &nodes_index);

  PROTECT_WITH_INDEX(names = Rf_getAttrib(nodes, R_NamesSymbol), &names_index);

  int n = LENGTH(nodes);

  REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), nodes_index);

  REPROTECT(names = Rf_lengthgets(names, n + 1), names_index);

  SET_VECTOR_ELT(nodes, n, node);

  SET_STRING_ELT(names, n, Rf_asChar(node));

  Rf_setAttrib(nodes, R_NamesSymbol, names);

  Rf_setVar(CG_NodesSymbol, nodes, graph);

  UNPROTECT(2);

  return n + 1;
}

void cg_add_value(SEXP node, SEXP value, SEXP graph)
{
  SEXP values = PROTECT(cg_get_values(graph));

  SEXP node_symbol = cg_get_symbol(node);

  Rf_defineVar(node_symbol, value, values);

  UNPROTECT(1);
}

SEXP cg_add_constant(SEXP value, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    if(!Rf_isString(name))
    {
      Rf_errorcall(R_NilValue, "name must be a character scalar");
    }

    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(CGCST, CHAR(Rf_asChar(node_name))));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(2);

  return node;
}

SEXP cg_add_input(SEXP value, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    if(!Rf_isString(name))
    {
      Rf_errorcall(R_NilValue, "name must be a character scalar");
    }

    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(CGIPT, CHAR(Rf_asChar(node_name))));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(2);

  return node;
}

SEXP cg_add_parameter(SEXP value, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    if(!Rf_isString(name))
    {
      Rf_errorcall(R_NilValue, "name must be a character scalar");
    }

    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(CGPRM, CHAR(Rf_asChar(node_name))));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(2);

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

    if(cg_get_type(node) == CGPRM)
    {
      SEXP node_symbol = cg_get_symbol(node);

      SEXP value = PROTECT(Rf_findVarInFrame(values, node_symbol));

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

SEXP cg_add_operation(SEXP call, SEXP grads, SEXP args, SEXP name, SEXP graph)
{
  SEXP node_name = R_NilValue;

  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "grads must be a list");
  }

  if(TYPEOF(args) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "args must be a list");
  }

  if(LENGTH(grads) != LENGTH(args))
  {
    Rf_errorcall(R_NilValue, "grads and args must be of the same length");
  }

  if(Rf_isNull(name))
  {
    node_name = PROTECT(cg_gen_name(graph));
  }
  else
  {
    if(!Rf_isString(name))
    {
      Rf_errorcall(R_NilValue, "name must be a character scalar");
    }

    node_name = PROTECT(Rf_duplicate(name));
  }

  SEXP node = PROTECT(cg_node(CGOPR, CHAR(Rf_asChar(node_name))));

  cg_set_call(node, call);

  int node_id = cg_add_node(node, graph);

  for(int i = 0; i < LENGTH(args); i++)
  {
    SEXP parent = VECTOR_ELT(args, i);

    cg_add_grad(parent, VECTOR_ELT(grads, i));

    cg_add_parent(node, cg_node_id(parent, graph));

    cg_add_child(parent, node_id);
  }

  UNPROTECT(2);

  return node;
}

int* cg_traverse_graph(int id, int* length, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = LENGTH(nodes);

  int* ids = malloc(n * sizeof(int));

  int* visited = calloc(n, sizeof(int));

  stack *s = stack_allocate(n);

  stack_add(s, id);

  (*length) = 0;

  while(!stack_is_empty(s))
  {
    int current = stack_current(s);

    SEXP node = PROTECT(cg_get_node_id(current, graph));

    if(visited[current - 1] == 0)
    {
      if(cg_has_parents(node))
      {
        int m;

        int* node_parents = cg_get_parents(node, &m);

        for(int i = 0; i < m; i++)
        {
          if(visited[node_parents[i] - 1] == 0)
          {
            stack_add(s, node_parents[i]);
          }
        }
      }
      else
      {
        ids[*length] = stack_get(s);

        (*length)++;
      }
    }
    else
    {
      if(visited[current - 1] == 1)
      {
        if(cg_has_parents(node))
        {
          ids[*length] = stack_get(s);

          (*length)++;
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

    UNPROTECT(1);
  }

  stack_destroy(s);

  free(visited);

  UNPROTECT(1);

  return ids;
}

SEXP cg_eval(SEXP node, SEXP values, SEXP graph)
{
  SEXP call = R_NilValue;

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(cg_get_type(node) == CGOPR)
  {
    int n, i = 0;

    int* node_parents = cg_get_parents(node, &n);

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

    for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
    {
      SEXP parent = PROTECT(cg_get_node_id(node_parents[i], graph));

      SETCAR(arg, cg_get_symbol(parent));

      UNPROTECT(1);

      //SEXP parent_value = PROTECT(Rf_eval(cg_get_symbol(parent), values));

      //SETCAR(arg, parent_value);

      //UNPROTECT(2);

      i++;
    }

    UNPROTECT(1);

    call = Rf_lcons(cg_get_call(node), args);
  }
  else
  {
    call = cg_get_symbol(node);
  }

  return Rf_eval(call, values);
}

SEXP cg_init_gradient(SEXP node, SEXP values, SEXP index, SEXP graph)
{
  SEXP call = R_NilValue;

  if(!Rf_isNumeric(index))
  {
    Rf_errorcall(R_NilValue, "index must be an numeric scalar");
  }

  SEXP node_value = PROTECT(Rf_eval(cg_get_symbol(node), values));

  SEXP args = PROTECT(Rf_allocVector(LISTSXP, 2));

  SETCAR(args, node_value);

  SET_TAG(CDR(args), Rf_install("index"));

  SETCAR(CDR(args), index);

  call = Rf_lcons(Rf_install("init.grad"), args);

  UNPROTECT(2);

  return Rf_eval(call, values);
}

SEXP cg_eval_gradient(SEXP node, SEXP values, SEXP grads, SEXP graph)
{
  int grad_index;

  SEXP grad = R_NilValue;

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isEnvironment(grads))
  {
    Rf_errorcall(R_NilValue, "grads must be an environment");
  }

  int n;

  int* node_childeren = cg_get_childeren(node, &n);

  for(int i = 0; i < n; i++)
  {
    SEXP child = PROTECT(cg_get_node_id(node_childeren[i], graph));

    SEXP child_grad = PROTECT(Rf_eval(cg_get_symbol(child), grads));

    int m, j = 0;

    int* child_parents = cg_get_parents(child, &m);

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, m + 1));

    SET_TAG(args, Rf_install("grad"));

    SETCAR(args, child_grad);

    for(SEXP arg = CDR(args); arg != R_NilValue; arg = CDR(arg))
    {
      SEXP parent = PROTECT(cg_get_node_id(child_parents[j], graph));

      SETCAR(arg, cg_get_symbol(parent));

      UNPROTECT(1);

      j++;
    }

    SEXP grad_call = PROTECT(Rf_lcons(cg_get_grad(node, i), args));

    if(i == 0)
    {
      PROTECT_WITH_INDEX(grad = Rf_eval(grad_call, values), &grad_index);
    }
    else
    {
      SEXP temp_call = PROTECT(Rf_lang3(Rf_install("+"), grad, grad_call));

      REPROTECT(grad = Rf_eval(temp_call, values), grad_index);

      UNPROTECT(1);
    }

    UNPROTECT(4);
  }

  UNPROTECT(1);

  return grad;
}

SEXP test(SEXP x, SEXP y, SEXP z)
{
  return Rf_lang3(x, y, z);
}


SEXP cg_eval_gradient2(SEXP node, SEXP values, SEXP grads, SEXP graph)
{
  SEXP grad = R_NilValue;

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isEnvironment(grads))
  {
    Rf_errorcall(R_NilValue, "grads must be an environment");
  }

  int n, i = 0;

  int* node_childeren = cg_get_childeren(node, &n);

  SEXP adjoints = PROTECT(Rf_allocVector(LISTSXP, n));

  for(SEXP adjoint = adjoints; adjoint != R_NilValue; adjoint = CDR(adjoint))
  {
    SEXP child = PROTECT(cg_get_node_id(node_childeren[i], graph));

    SEXP child_grad = PROTECT(Rf_eval(cg_get_symbol(child), grads));

    int m, j = 0;

    int* child_parents = cg_get_parents(child, &m);

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, m + 1));

    SET_TAG(args, Rf_install("grad"));

    SETCAR(args, child_grad);

    for(SEXP arg = CDR(args); arg != R_NilValue; arg = CDR(arg))
    {
      SEXP parent = PROTECT(cg_get_node_id(child_parents[j], graph));

      SETCAR(arg, cg_get_symbol(parent));

      UNPROTECT(1);

      j++;
    }

    SEXP adjoint_call = PROTECT(Rf_lcons(cg_get_grad(node, i), args));

    SEXP adjoint_value = PROTECT(Rf_eval(adjoint_call, values));

    SETCAR(adjoint, adjoint_value);

    UNPROTECT(5);

    i++;
  }

  if(n < 2)
  {
    grad = PROTECT(CAR(adjoints));
  }
  else
  {
    int grad_index;

    SEXP test2 = PROTECT(Rf_list2(CAR(adjoints), CADR(adjoints)));

    SEXP test3 = PROTECT(Rf_lcons(Rf_install("+"), test2));

    PROTECT_WITH_INDEX(grad = Rf_eval(test3, values), &grad_index);

    for(SEXP adjoint = CDDR(adjoints); adjoint != R_NilValue; adjoint = CDR(adjoint))
    {
      SEXP test4 = PROTECT(Rf_list2(grad, CAR(adjoint)));

      SEXP test5 = PROTECT(Rf_lcons(Rf_install("+"), test4));

      REPROTECT(grad = Rf_eval(test5, values), grad_index);

      UNPROTECT_PTR(test4);
      UNPROTECT_PTR(test5);
    }

    UNPROTECT_PTR(test2);
    UNPROTECT_PTR(test3);
  }

  UNPROTECT(2);

  return grad;
}

SEXP cg_run(SEXP name, SEXP values, SEXP graph)
{
  int n = 0, id = cg_node_id(name, graph);

  int* ids = cg_traverse_graph(id, &n, graph);

  SEXP default_values = PROTECT(cg_get_values(graph));

  if(values != default_values)
  {
    SET_ENCLOS(default_values, values);
  }

  for(int i = 0; i < n; i++)
  {
    SEXP node = cg_get_node_id(ids[i], graph);

    SEXP node_value = PROTECT(cg_eval(node, default_values, graph));

    Rf_defineVar(cg_get_symbol(node), node_value, values);

    UNPROTECT(1);
  }

  SET_ENCLOS(default_values, R_EmptyEnv);

  free(ids);

  UNPROTECT(1);

  return values;
}

SEXP cg_gradients(SEXP name, SEXP values, SEXP grads, SEXP index, SEXP graph)
{
  int n = 0, id = cg_node_id(name, graph);

  int* ids = cg_traverse_graph(id, &n, graph);

  SEXP default_values = PROTECT(cg_get_values(graph));

  if(values != default_values)
  {
    SET_ENCLOS(default_values, values);
  }

  if(n > 0)
  {
    SEXP root = cg_get_node_id(ids[n - 1], graph);

    SEXP root_grad = PROTECT(cg_init_gradient(root, values, index, graph));

    Rf_defineVar(cg_get_symbol(root), root_grad, grads);

    UNPROTECT(1);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = cg_get_node_id(ids[i], graph);

      SEXP node_grad = PROTECT(cg_eval_gradient(node, values, grads, graph));

      Rf_defineVar(cg_get_symbol(node), node_grad, grads);

      UNPROTECT(1);
    }
  }

  SET_ENCLOS(default_values, R_EmptyEnv);

  free(ids);

  UNPROTECT(1);

  return grads;
}

/*

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

  //SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  if(values != graph_values)
  {
    SET_ENCLOS(values, graph_values);
  }

  //cg_forward(ids, values, graph);

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

  //SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  if(values != graph_values)
  {
    SET_ENCLOS(values, graph_values);
  }

  //cg_backward(ids, index, values, grads, graph);

  SET_ENCLOS(values, enclos);

  UNPROTECT(4);

  return grads;
}

*/

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

  //SEXP ids = PROTECT(cg_traverse_graph(x, graph));

  SEXP x_node = VECTOR_ELT(nodes, cg_node_id(x, graph) - 1);
  SEXP y_node = VECTOR_ELT(nodes, cg_node_id(y, graph) - 1);

  if(values != graph_values)
  {
    SET_ENCLOS(values, graph_values);
  }

  //cg_forward(ids, values, graph);

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

    //cg_forward(ids, values, graph);

    PROTECT_WITH_INDEX(x_value1 = Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values), &ipx1);

    if(!Rf_isReal(x_value1))
    {
      REPROTECT(x_value1 = Rf_coerceVector(x_value1, REALSXP), ipx1);
    }

    REAL(y_value)[i] -= 2 * Rf_asReal(eps);

    //cg_forward(ids, values, graph);

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
