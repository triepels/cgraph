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

SEXP cgraph(SEXP graph, SEXP values)
{
  SEXP nodes = PROTECT(Rf_allocVector(VECSXP, 0));

  if(!Rf_isEnvironment(graph))
  {
    Rf_errorcall(R_NilValue, "graph must be an environment");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  Rf_setAttrib(nodes, R_NamesSymbol, Rf_allocVector(STRSXP, 0));

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

int cg_has_value(SEXP node, SEXP values)
{
  int has_value = FALSE;

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  SEXP node_symbol = cg_get_symbol(node);

  SEXP node_value = PROTECT(Rf_findVarInFrame3(values, node_symbol, FALSE));

  if(node_value != R_UnboundValue)
  {
    has_value = TRUE;
  }

  UNPROTECT(1);

  return has_value;
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
  SEXP names = Rf_getAttrib(parms, R_NamesSymbol);

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

    cg_add_grad(parent, VECTOR_ELT(grads, i));

    cg_add_parent(node, cg_node_id(parent, graph));

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

      if(m > 0 && !cg_has_value(node, values))
      {
        for(int i = 0; i < m; i++)
        {
          if(p_visited[p_node_parents[i] - 1] == 0)
          {
            stack_add(s, p_node_parents[i]);
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

    int* p_node_parents = cg_get_parents(node, &n);

    SEXP args = PROTECT(Rf_allocVector(LISTSXP, n));

    for(SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
    {
      SEXP parent = PROTECT(cg_get_node_id(p_node_parents[i], graph));

      SETCAR(arg, cg_get_symbol(parent));

      UNPROTECT(1);

      i++;
    }

    call = Rf_lcons(cg_get_call(node), args);

    UNPROTECT(1);
  }
  else
  {
    call = cg_get_symbol(node);
  }

  return Rf_eval(call, values);
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
        Rf_errorcall(R_NilValue, "invalid index provided");
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
        Rf_errorcall(R_NilValue, "invalid index provided");
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
  SEXP grad;

  int i_grad;

  PROTECT_WITH_INDEX(grad = R_NilValue, &i_grad);

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

    SEXP child_grad = PROTECT(Rf_eval(child_symbol, grads));

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

    if(i == 0)
    {
      REPROTECT(grad = Rf_eval(call, values), i_grad);

      l = length(grad);
    }
    else
    {
      SEXP value = PROTECT(Rf_eval(call, values));

      switch(TYPEOF(grad))
      {
        case LGLSXP :
        case INTSXP :
        {
          switch(TYPEOF(value))
          {
            case LGLSXP :
            case INTSXP :
            {
              int* p_grad = INTEGER(grad);

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
              int* p_grad = INTEGER(grad);

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
              double* p_grad = REAL(grad);

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
              double* p_grad = REAL(grad);

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
          Rf_errorcall(R_NilValue, "cannot differentiate object of type '%s'", Rf_type2char(TYPEOF(grad)));
        }
      }

      UNPROTECT(1);
    }

    UNPROTECT(4);
  }

  UNPROTECT(1);

  return grad;
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
  SEXP graph_values = PROTECT(cg_get_values(graph));

  int node_id = cg_node_id(name, graph), n;

  int* p_ids = cg_forward_dep(node_id, &n, graph);

  for(int i = 0; i < n; i++)
  {
    SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

    Rf_setVar(cg_get_symbol(node), R_UnboundValue, graph_values);

    UNPROTECT(1);
  }

  Rf_setVar(cg_get_symbol(name), value, graph_values);

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

  SEXP graph_values = PROTECT(cg_get_values(graph));

  SEXP graph_values_enclos = PROTECT(ENCLOS(graph_values));

  if(values != graph_values)
  {
    SET_ENCLOS(graph_values, values);
  }

  for(int i = 0; i < n; i++)
  {
    SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

    SEXP node_value = PROTECT(cg_eval(node, graph_values, graph));

    Rf_defineVar(cg_get_symbol(node), node_value, values);

    UNPROTECT(2);
  }

  SET_ENCLOS(graph_values, graph_values_enclos);

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

  SEXP graph_values = PROTECT(cg_get_values(graph));

  SEXP graph_values_enclos = PROTECT(ENCLOS(graph_values));

  if(values != graph_values)
  {
    SET_ENCLOS(graph_values, values);
  }

  if(n > 0)
  {
    SEXP root = PROTECT(cg_get_node_id(p_ids[n - 1], graph));

    SEXP root_grad = PROTECT(cg_init_gradient(root, graph_values, index, graph));

    Rf_defineVar(cg_get_symbol(root), root_grad, grads);

    UNPROTECT(2);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = PROTECT(cg_get_node_id(p_ids[i], graph));

      SEXP node_grad = PROTECT(cg_eval_gradient(node, graph_values, grads, graph));

      Rf_defineVar(cg_get_symbol(node), node_grad, grads);

      UNPROTECT(2);
    }
  }

  SET_ENCLOS(graph_values, graph_values_enclos);

  UNPROTECT(2);

  free(p_ids);

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
