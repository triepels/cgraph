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

#define CG_TypeSymbol Rf_install("cg_type")
#define CG_CallSymbol Rf_install("cg_call")
#define CG_GradsSymbol Rf_install("cg_grads")
#define CG_ParentsSymbol Rf_install("cg_parents")
#define CG_ChilderenSymbol Rf_install("cg_childeren")

SEXP cgraph(SEXP graph, SEXP values, SEXP library)
{
  if(!Rf_isEnvironment(graph))
  {
    Rf_errorcall(R_NilValue, "graph must be an environment");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isEnvironment(library))
  {
    Rf_errorcall(R_NilValue, "library must be an environment");
  }

  if(library == values)
  {
    Rf_errorcall(R_NilValue, "library cannot be the same environment as values");
  }

  SEXP nodes = PROTECT(Rf_allocVector(VECSXP, 0));

  Rf_setAttrib(nodes, R_NamesSymbol, Rf_allocVector(STRSXP, 0));

  SET_ENCLOS(values, library);

  Rf_defineVar(CG_NodesSymbol, nodes, graph);
  Rf_defineVar(CG_ValuesSymbol, values, graph);

  Rf_setAttrib(graph, R_ClassSymbol, Rf_mkString("cgraph"));

  UNPROTECT(1);

  return graph;
}

SEXP cg_get_nodes(SEXP graph)
{
  if(!Rf_isEnvironment(graph))
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
  if(!Rf_isEnvironment(graph))
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
    Rf_errorcall(R_NilValue, "invalid node provided");
  }

  return CHAR(STRING_ELT(node, 0));
}

void cg_set_name(SEXP node, const char* name)
{
  if(!Rf_isString(node))
  {
    Rf_errorcall(R_NilValue, "invalid node provided");
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

  if(!Rf_isInteger(node_type) || LENGTH(node_type) < 1)
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

  SEXP node_type = PROTECT(Rf_ScalarInteger(type));

  Rf_setAttrib(node, CG_TypeSymbol, node_type);

  UNPROTECT(1);
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

  int n = LENGTH(node_grads);

  for(int i = 0; i < n; i++)
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

  int n = LENGTH(grads);

  for(int i = 0; i < n; i++)
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
  int i_node_grads;

  SEXP node_grads = R_NilValue;

  if(!Rf_isSymbol(grad))
  {
    Rf_errorcall(R_NilValue, "grad must be a symbol");
  }

  PROTECT_WITH_INDEX(node_grads = Rf_getAttrib(node, CG_GradsSymbol), &i_node_grads);

  int index = 0;

  if(TYPEOF(node_grads) != VECSXP)
  {
    REPROTECT(node_grads = Rf_allocVector(VECSXP, 1), i_node_grads);

    SET_VECTOR_ELT(node_grads, index, grad);
  }
  else
  {
    index = LENGTH(node_grads);

    REPROTECT(node_grads = Rf_lengthgets(node_grads, index + 1), i_node_grads);

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

int* cg_get_parents(SEXP node, int* p_length)
{
  int* p_node_parents = NULL;

  SEXP node_parents = Rf_getAttrib(node, CG_ParentsSymbol);

  if(!Rf_isInteger(node_parents))
  {
    (*p_length) = 0;
  }
  else
  {
    p_node_parents = INTEGER(node_parents);

    (*p_length) = LENGTH(node_parents);
  }

  return p_node_parents;
}

int cg_add_parent(SEXP node, int id)
{
  int i_node_parents;

  SEXP node_parents = R_NilValue;

  PROTECT_WITH_INDEX(node_parents = Rf_getAttrib(node, CG_ParentsSymbol), &i_node_parents);

  int index = 0;

  if(!Rf_isInteger(node_parents))
  {
    REPROTECT(node_parents = Rf_ScalarInteger(id), i_node_parents);
  }
  else
  {
    index = LENGTH(node_parents);

    REPROTECT(node_parents = Rf_lengthgets(node_parents, index + 1), i_node_parents);

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

int* cg_get_childeren(SEXP node, int* p_length)
{
  int* p_node_childeren = NULL;

  SEXP node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol);

  if(!Rf_isInteger(node_childeren))
  {
    (*p_length) = 0;
  }
  else
  {
    p_node_childeren = INTEGER(node_childeren);

    (*p_length) = LENGTH(node_childeren);
  }

  return p_node_childeren;
}

int cg_add_child(SEXP node, int id)
{
  int i_node_childeren;

  SEXP node_childeren = R_NilValue;

  PROTECT_WITH_INDEX(node_childeren = Rf_getAttrib(node, CG_ChilderenSymbol), &i_node_childeren);

  int index = 0;

  if(!Rf_isInteger(node_childeren))
  {
    REPROTECT(node_childeren = Rf_ScalarInteger(id), i_node_childeren);
  }
  else
  {
    index = LENGTH(node_childeren);

    REPROTECT(node_childeren = Rf_lengthgets(node_childeren, index + 1), i_node_childeren);

    INTEGER(node_childeren)[index] = id;
  }

  Rf_setAttrib(node, CG_ChilderenSymbol, node_childeren);

  UNPROTECT(1);

  return index;
}

SEXP cg_node(int type, const char* name)
{
  SEXP node = PROTECT(Rf_allocVector(STRSXP, 1));

  Rf_setAttrib(node, R_ClassSymbol, Rf_mkString("cg_node"));

  cg_set_type(node, type);

  cg_set_name(node, name);

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

  int n = LENGTH(nodes);

  for(int i = 0; i < n; i++)
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

SEXP cg_gen_name(SEXP graph)
{
  char name[32];

  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = Rf_xlength(nodes), i = 0;

  do
  {
    i++;

    sprintf(name, "node%d", n + i);
  }
  while (cg_node_exists(name, graph));

  UNPROTECT(1);

  return Rf_mkString(name);
}

int cg_add_node(SEXP node, SEXP graph)
{
  const char* node_name = cg_get_name(node);

  if(cg_node_exists(node_name, graph))
  {
    Rf_errorcall(R_NilValue, "'%s' is already defined in the graph", node_name);
  }

  int i_nodes, i_names;

  SEXP nodes = R_NilValue, names = R_NilValue;

  PROTECT_WITH_INDEX(nodes = cg_get_nodes(graph), &i_nodes);

  PROTECT_WITH_INDEX(names = Rf_getAttrib(nodes, R_NamesSymbol), &i_names);

  int n = LENGTH(nodes);

  REPROTECT(nodes = Rf_lengthgets(nodes, n + 1), i_nodes);

  REPROTECT(names = Rf_lengthgets(names, n + 1), i_names);

  SET_VECTOR_ELT(nodes, n, node);

  SET_STRING_ELT(names, n, Rf_asChar(node));

  Rf_setAttrib(nodes, R_NamesSymbol, names);

  Rf_setVar(CG_NodesSymbol, nodes, graph);

  UNPROTECT(2);

  return n + 1;
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
    SEXP values = PROTECT(cg_get_values(graph));

    Rf_defineVar(cg_get_symbol(node), value, values);

    UNPROTECT(1);
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
    SEXP values = PROTECT(cg_get_values(graph));

    Rf_defineVar(cg_get_symbol(node), value, values);

    UNPROTECT(1);
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
    SEXP values = PROTECT(cg_get_values(graph));

    Rf_defineVar(cg_get_symbol(node), value, values);

    UNPROTECT(1);
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
  SEXP names = PROTECT(Rf_getAttrib(parms, R_NamesSymbol));

  if(TYPEOF(parms) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "parms must be a named list");
  }

  int n = LENGTH(parms);

  for(int i = 0; i < n; i++)
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

  UNPROTECT(1);

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

  int n = LENGTH(args);

  for(int i = 0; i < n; i++)
  {
    SEXP parent = VECTOR_ELT(args, i);

    cg_add_parent(node, cg_node_id(parent, graph));

    cg_add_grad(parent, VECTOR_ELT(grads, i));

    cg_add_child(parent, node_id);
  }

  UNPROTECT(2);

  return node;
}

int* cg_forward_dep(int id, int* p_length, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = LENGTH(nodes);

  int* p_ids = malloc(n * sizeof(int));

  int* p_visited = calloc(n, sizeof(int));

  stack *s = stack_allocate(n);

  stack_add(s, id);

  (*p_length) = 0;

  while(!stack_is_empty(s))
  {
    int current = stack_current(s);

    SEXP node = PROTECT(cg_get_node_id(current, graph));

    if(p_visited[current - 1] == 0)
    {
      int m;

      int* node_childeren = cg_get_childeren(node, &m);

      if(m > 0)
      {
        for(int i = 0; i < m; i++)
        {
          if(p_visited[node_childeren[i] - 1] == 0)
          {
            stack_add(s, node_childeren[i]);
          }
        }
      }
      else
      {
        p_ids[*p_length] = stack_get(s);

        (*p_length)++;
      }
    }
    else
    {
      if(p_visited[current - 1] == 1)
      {
        if(cg_has_childeren(node))
        {
          p_ids[*p_length] = stack_get(s);

          (*p_length)++;
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

    p_visited[current - 1] += 1;

    UNPROTECT(1);
  }

  stack_destroy(s);

  free(p_visited);

  UNPROTECT(1);

  return p_ids;
}

int* cg_backward_dep(int id, int* p_length, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = LENGTH(nodes);

  int* p_ids = malloc(n * sizeof(int));

  int* p_visited = calloc(n, sizeof(int));

  stack *s = stack_allocate(n);

  stack_add(s, id);

  (*p_length) = 0;

  while(!stack_is_empty(s))
  {
    int current = stack_current(s);

    SEXP node = PROTECT(cg_get_node_id(current, graph));

    if(p_visited[current - 1] == 0)
    {
      int m;

      int* node_parents = cg_get_parents(node, &m);

      if(m > 0)
      {
        for(int i = 0; i < m; i++)
        {
          if(p_visited[node_parents[i] - 1] == 0)
          {
            stack_add(s, node_parents[i]);
          }
        }
      }
      else
      {
        p_ids[*p_length] = stack_get(s);

        (*p_length)++;
      }
    }
    else
    {
      if(p_visited[current - 1] == 1)
      {
        if(cg_has_parents(node))
        {
          p_ids[*p_length] = stack_get(s);

          (*p_length)++;
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

    p_visited[current - 1] += 1;

    UNPROTECT(1);
  }

  stack_destroy(s);

  free(p_visited);

  UNPROTECT(1);

  return p_ids;
}

int* cg_unset_backward_dep(int id, int* p_length, SEXP values, SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = LENGTH(nodes);

  int* p_ids = malloc(n * sizeof(int));

  int* p_visited = calloc(n, sizeof(int));

  stack *s = stack_allocate(n);

  stack_add(s, id);

  (*p_length) = 0;

  while(!stack_is_empty(s))
  {
    int current = stack_current(s);

    SEXP node = PROTECT(cg_get_node_id(current, graph));

    if(p_visited[current - 1] == 0)
    {
      int m;

      int* p_node_parents = cg_get_parents(node, &m);

      if(m > 0)
      {
        for(int i = 0; i < m; i++)
        {
          SEXP parent = PROTECT(cg_get_node_id(p_node_parents[i], graph));

          SEXP parent_value = PROTECT(Rf_findVarInFrame3(values, cg_get_symbol(parent), FALSE));

          if(p_visited[p_node_parents[i] - 1] == 0 && parent_value == R_UnboundValue)
          {
            stack_add(s, p_node_parents[i]);
          }

          UNPROTECT(2);
        }
      }
      else
      {
        p_ids[*p_length] = stack_get(s);

        (*p_length)++;
      }
    }
    else
    {
      if(p_visited[current - 1] == 1)
      {
        if(cg_has_parents(node))
        {
          p_ids[*p_length] = stack_get(s);

          (*p_length)++;
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

    p_visited[current - 1] += 1;

    UNPROTECT(1);
  }

  stack_destroy(s);

  free(p_visited);

  UNPROTECT(1);

  return p_ids;
}

SEXP cg_eval(SEXP node, SEXP values, SEXP graph)
{
  int i_node_value;

  SEXP node_value;

  PROTECT_WITH_INDEX(node_value = R_NilValue, &i_node_value);

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(cg_get_type(node) == CGOPR)
  {
    int n, i = 0;

    int* p_node_parents = cg_get_parents(node, &n);

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

    for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
    {
      SEXP parent = PROTECT(cg_get_node_id(p_node_parents[i], graph));

      SETCAR(arg, cg_get_symbol(parent));

      UNPROTECT(1);

      i++;
    }

    SEXP call = PROTECT(Rf_lcons(cg_get_call(node), args));

    REPROTECT(node_value = Rf_eval(call, values), i_node_value);

    UNPROTECT(2);
  }
  else
  {
    SEXP call = cg_get_symbol(node);

    REPROTECT(node_value = Rf_eval(call, values), i_node_value);
  }

  UNPROTECT(1);

  return node_value;
}

SEXP cg_init_gradient(SEXP node, SEXP values, SEXP index, SEXP graph)
{
  int i_node_grad;

  SEXP node_grad = R_NilValue;

  if(!Rf_isNumeric(index))
  {
    Rf_errorcall(R_NilValue, "index must be an numeric scalar");
  }

  PROTECT_WITH_INDEX(node_grad = Rf_eval(cg_get_symbol(node), values), &i_node_grad);

  REPROTECT(node_grad = Rf_duplicate(node_grad), i_node_grad);

  int x_index = Rf_asInteger(index);

  switch(TYPEOF(node_grad))
  {
    case LGLSXP :
    case INTSXP :
    {
      int n = LENGTH(node_grad);

      if(x_index < 1 || x_index > n)
      {
        Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at index %d", cg_get_name(node), x_index);
      }

      int* p_node_grad = INTEGER(node_grad);

      memset(p_node_grad, 0, n * sizeof(int));

      p_node_grad[x_index - 1] = 1;

      break;
    }
    case REALSXP :
    {
      int n = LENGTH(node_grad);

      if(x_index < 1 || x_index > n)
      {
        Rf_errorcall(R_NilValue, "cannot differentiate node '%s' at index %d", cg_get_name(node), x_index);
      }

      double* p_node_grad = REAL(node_grad);

      memset(p_node_grad, 0, n * sizeof(double));

      p_node_grad[x_index - 1] = 1;

      break;
    }
    default :
    {
      Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s'", Rf_type2char(TYPEOF(node_grad)));
    }
  }

  UNPROTECT(1);

  return node_grad;
}

SEXP cg_eval_gradient(SEXP node, SEXP values, SEXP grads, SEXP graph)
{
  int i_node_grad;

  SEXP node_grad;

  PROTECT_WITH_INDEX(node_grad = R_NilValue, &i_node_grad);

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isEnvironment(grads))
  {
    Rf_errorcall(R_NilValue, "grads must be an environment");
  }

  int n, l = 0;

  int* p_node_childeren = cg_get_childeren(node, &n);

  for(int i = 0; i < n; i++)
  {
    SEXP child = PROTECT(cg_get_node_id(p_node_childeren[i], graph));

    SEXP child_symbol = cg_get_symbol(child);

    SEXP child_grad = PROTECT(Rf_findVarInFrame(grads, child_symbol));

    if(child_grad != R_UnboundValue)
    {
      int m;

      int* p_child_parents = cg_get_parents(child, &m);

      SEXP args = PROTECT(Rf_allocVector(LISTSXP, m + 2));

      SEXP arg = args;

      for(int j = 0; j < m; j++)
      {
        SEXP parent = PROTECT(cg_get_node_id(p_child_parents[j], graph));

        SETCAR(arg, cg_get_symbol(parent));

        arg = CDR(arg);

        UNPROTECT(1);
      }

      SET_TAG(arg, Rf_install("val"));

      SETCAR(arg, child_symbol);

      SET_TAG(CDR(arg), Rf_install("grad"));

      SETCADR(arg, child_grad);

      SEXP call = PROTECT(Rf_lcons(cg_get_grad(node, i), args));

      if(Rf_isNull(node_grad))
      {
        REPROTECT(node_grad = Rf_eval(call, values), i_node_grad);

        l = length(node_grad);
      }
      else
      {
        SEXP value = PROTECT(Rf_eval(call, values));

        switch(TYPEOF(node_grad))
        {
          case LGLSXP :
          case INTSXP :
          {
            switch(TYPEOF(value))
            {
              case LGLSXP :
              case INTSXP :
              {
                int* p_grad = INTEGER(node_grad);

                int* p_value = INTEGER(value);

                if(LENGTH(value) != l)
                {
                  Rf_errorcall(R_NilValue, "cannot accumulate gradient of node '%s' at index %d", cg_get_name(node), i);
                }

                for(int k = 0; k < l; k++)
                {
                  p_grad[k] += p_value[k];
                }

                break;
              }
              case REALSXP :
              {
                int* p_grad = INTEGER(node_grad);

                double* p_value = REAL(value);

                if(LENGTH(value) != l)
                {
                  Rf_errorcall(R_NilValue, "cannot accumulate gradient of node '%s' at index %d", cg_get_name(node), i);
                }

                for(int k = 0; k < l; k++)
                {
                  p_grad[k] += p_value[k];
                }

                break;
              }
              default :
              {
                Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s'", Rf_type2char(TYPEOF(value)));
              }
            }

            break;
          }
          case REALSXP :
          {
            switch(TYPEOF(value))
            {
              case LGLSXP :
              case INTSXP :
              {
                double* p_grad = REAL(node_grad);

                int* p_value = INTEGER(value);

                if(LENGTH(value) != l)
                {
                  Rf_errorcall(R_NilValue, "cannot accumulate gradient of node '%s' at index %d", cg_get_name(node), i);
                }

                for(int k = 0; k < l; k++)
                {
                  p_grad[k] += p_value[k];
                }

                break;
              }
              case REALSXP :
              {
                double* p_grad = REAL(node_grad);

                double* p_value = REAL(value);

                if(LENGTH(value) != l)
                {
                  Rf_errorcall(R_NilValue, "cannot accumulate gradient of node '%s' at index %d", cg_get_name(node), i);
                }

                for(int k = 0; k < l; k++)
                {
                  p_grad[k] += p_value[k];
                }

                break;
              }
              default :
              {
                Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s'", Rf_type2char(TYPEOF(value)));
              }
            }

            break;
          }
          default :
          {
            Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s'", Rf_type2char(TYPEOF(node_grad)));
          }
        }

        UNPROTECT(1);
      }

      UNPROTECT(2);
    }

    UNPROTECT(2);
  }

  UNPROTECT(1);

  return node_grad;
}

SEXP cg_get(SEXP name, SEXP graph)
{
  SEXP node_value;

  SEXP graph_values = PROTECT(cg_get_values(graph));

  int node_id = cg_node_id(name, graph), i_node_value, n;

  int* p_ids = cg_unset_backward_dep(node_id, &n, graph_values, graph);

  PROTECT_WITH_INDEX(node_value = R_NilValue, &i_node_value);

  for(int i = 0; i < n; i++)
  {
    SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

    REPROTECT(node_value = cg_eval(node, graph_values, graph), i_node_value);

    Rf_defineVar(cg_get_symbol(node), node_value, graph_values);

    UNPROTECT(1);
  }

  UNPROTECT(2);

  free(p_ids);

  return node_value;
}

SEXP cg_set(SEXP name, SEXP value, SEXP graph)
{
  int node_id = cg_node_id(name, graph), n;

  int* p_ids = cg_forward_dep(node_id, &n, graph);

  SEXP graph_values = PROTECT(cg_get_values(graph));

  for(int i = n - 2; i >= 0; i--)
  {
    SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

    Rf_defineVar(cg_get_symbol(node), R_UnboundValue, graph_values);

    UNPROTECT(1);
  }

  Rf_defineVar(cg_get_symbol(name), value, graph_values);

  UNPROTECT(1);

  free(p_ids);

  return R_NilValue;
}

SEXP cg_run(SEXP name, SEXP values, SEXP graph)
{
  int node_id = cg_node_id(name, graph), n;

  int* p_ids = cg_backward_dep(node_id, &n, graph);

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  SEXP values_enclos = PROTECT(ENCLOS(values));

  SEXP graph_values = PROTECT(cg_get_values(graph));

  if(graph_values != values)
  {
    SET_ENCLOS(values, graph_values);
  }

  for(int i = 0; i < n; i++)
  {
    SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

    int node_type = cg_get_type(node);

    if(node_type == CGOPR)
    {
      SEXP node_value = PROTECT(cg_eval(node, values, graph));

      Rf_defineVar(cg_get_symbol(node), node_value, values);

      UNPROTECT(1);
    }

    UNPROTECT(1);
  }

  SET_ENCLOS(values, values_enclos);

  UNPROTECT(2);

  free(p_ids);

  return values;
}

SEXP cg_gradients(SEXP name, SEXP values, SEXP grads, SEXP index, SEXP graph)
{
  int node_id = cg_node_id(name, graph), n;

  int* p_ids = cg_backward_dep(node_id, &n, graph);

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  if(!Rf_isEnvironment(grads))
  {
    Rf_errorcall(R_NilValue, "grads must be an environment");
  }

  SEXP values_enclos = PROTECT(ENCLOS(values));

  SEXP graph_values = PROTECT(cg_get_values(graph));

  if(graph_values != values)
  {
    SET_ENCLOS(values, graph_values);
  }

  if(n > 0)
  {
    SEXP root = PROTECT(cg_get_node_id(p_ids[n - 1], graph));

    SEXP root_grad = PROTECT(cg_init_gradient(root, values, index, graph));

    Rf_defineVar(cg_get_symbol(root), root_grad, grads);

    UNPROTECT(2);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

      int node_type = cg_get_type(node);

      if(node_type == CGPRM || node_type == CGOPR)
      {
        SEXP node_grad = PROTECT(cg_eval_gradient(node, values, grads, graph));

        Rf_defineVar(cg_get_symbol(node), node_grad, grads);

        UNPROTECT(1);
      }

      UNPROTECT(1);
    }
  }

  SET_ENCLOS(values, values_enclos);

  UNPROTECT(2);

  free(p_ids);

  return grads;
}

SEXP cg_adj_mat(SEXP graph)
{
  SEXP nodes = PROTECT(cg_get_nodes(graph));

  int n = LENGTH(nodes);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));

  SEXP mat = PROTECT(Rf_allocMatrix(INTSXP, n, n));

  int* p_mat = INTEGER(mat);

  memset(p_mat, 0, n * n * sizeof(int));

  for(int i = 0; i < n; i++)
  {
    SEXP node = PROTECT(cg_get_node_id(i + 1, graph));

    int m;

    int* p_node_childeren = cg_get_childeren(node, &m);

    for(int j = 0; j < m; j++)
    {
      p_mat[i + n * (p_node_childeren[j] - 1)] = 1;
    }

    SET_STRING_ELT(names, i, Rf_asChar(node));

    UNPROTECT(1);
  }

  SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(dimnames, 0, names);
  SET_VECTOR_ELT(dimnames, 1, names);

  Rf_setAttrib(mat, R_DimNamesSymbol, dimnames);

  UNPROTECT(4);

  return mat;
}
