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

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "stack.h"

#define CGCST 0
#define CGIPT 1
#define CGPRM 2
#define CGOPR 3

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
  SEXP nodes = PROTECT(Rf_allocVector(VECSXP, 0));

  if(Rf_isNull(graph) || !Rf_isEnvironment(graph))
  {
    graph = NewEnv(R_EmptyEnv);
  }

  if(Rf_isNull(values) || !Rf_isEnvironment(values))
  {
    values = NewEnv(R_BaseEnv);
  }

  Rf_setAttrib(nodes, R_NamesSymbol, Rf_allocVector(STRSXP, 0));

  Rf_defineVar(Rf_install("nodes"), nodes, graph);
  Rf_defineVar(Rf_install("values"), values, graph);

  Rf_setAttrib(graph, R_ClassSymbol, Rf_mkString("cgraph"));

  UNPROTECT(1);

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

static SEXP cg_find_nodes(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    Rf_errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP nodes = PROTECT(Rf_findVarInFrame(graph, Rf_install("nodes")));

  SEXP names = Rf_getAttrib(nodes, R_NamesSymbol);

  if(TYPEOF(nodes) != VECSXP || !Rf_isString(names))
  {
    Rf_errorcall(R_NilValue, "cannot find valid nodes object in cgraph object environment");
  }

  UNPROTECT(1);

  return nodes;
}

static SEXP cg_find_values(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    Rf_errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP values = PROTECT(Rf_findVarInFrame(graph, Rf_install("values")));

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "cannot find valid values object in cgraph object environment");
  }

  UNPROTECT(1);

  return values;
}

static void cg_add_node(SEXP node, SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

  SEXP names = Rf_getAttrib(nodes, R_NamesSymbol);

  if(!is_cg_node(node))
  {
    Rf_errorcall(R_NilValue, "invalid cg.node object provided");
  }

  int n = LENGTH(nodes);

  nodes = PROTECT(Rf_lengthgets(nodes, n + 1));

  names = PROTECT(Rf_lengthgets(names, n + 1));

  SET_VECTOR_ELT(nodes, n, node);

  SET_STRING_ELT(names, n, Rf_asChar(node));

  Rf_setAttrib(nodes, R_NamesSymbol, names);

  Rf_setVar(Rf_install("nodes"), nodes, graph);

  UNPROTECT(3);
}

static void cg_add_value(SEXP node, SEXP value, SEXP graph)
{
  SEXP values = PROTECT(cg_find_values(graph));

  if(!is_cg_node(node))
  {
    Rf_errorcall(R_NilValue, "invalid cg.node object provided");
  }

  if(!Rf_isNumeric(value))
  {
    Rf_errorcall(R_NilValue, "node '%s' does not evaluate to a numeric vector or array", CHAR(Rf_asChar(node)));
  }

  if(Rf_isInteger(value))
  {
    value = Rf_coerceVector(value, REALSXP);
  }

  Rf_defineVar(Rf_install(CHAR(Rf_asChar(node))), value, values);

  UNPROTECT(1);
}

SEXP cg_gen_name(SEXP graph)
{
  char name[32];

  SEXP nodes = PROTECT(cg_find_nodes(graph));

  sprintf(name, "node%d", LENGTH(nodes) + 1);

  UNPROTECT(1);

  return Rf_mkString(name);
}

int cg_node_id(SEXP name, SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

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

int cg_node_exists(SEXP name, SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

  if(!(Rf_isString(name) || Rf_isSymbol(name)))
  {
    Rf_errorcall(R_NilValue, "name must be a character scalar or symbol");
  }

  for(int i = 0; i < LENGTH(nodes); i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(CHAR(Rf_asChar(node)), CHAR(Rf_asChar(name))) == 0)
    {
      UNPROTECT(1);

      return TRUE;
    }
  }

  UNPROTECT(1);

  return FALSE;
}

static SEXP cg_node(SEXP name, SEXP type, SEXP graph)
{
  SEXP node = R_NilValue;

  if(Rf_isNull(name))
  {
    node = PROTECT(cg_gen_name(graph));
  }
  else
  {
    if(!Rf_isString(name) || Rf_asChar(name) == R_BlankString)
    {
      Rf_errorcall(R_NilValue, "the name of a node must be a non-blank character scalar");
    }

    node = PROTECT(Rf_mkString(CHAR(Rf_asChar(name))));
  }

  if(cg_node_exists(node, graph))
  {
    Rf_errorcall(R_NilValue, "node '%s' is already defined", CHAR(Rf_asChar(node)));
  }

  if(strcmp(CHAR(Rf_asChar(node)), "grad") == 0)
  {
    Rf_errorcall(R_NilValue, "'grad' is a reserved word that cannot be used");
  }

  if(!Rf_isNumber(type))
  {
    Rf_errorcall(R_NilValue, "type must be an numeric scalar");
  }

  switch(Rf_asInteger(type))
  {
    case CGCST :
      Rf_setAttrib(node, Rf_install("type"), Rf_ScalarInteger(CGCST));
      Rf_setAttrib(node, Rf_install("grads"), Rf_allocVector(VECSXP, 0));
      Rf_setAttrib(node, Rf_install("childeren"), Rf_allocVector(INTSXP, 0));
      break;

    case CGIPT :
      Rf_setAttrib(node, Rf_install("type"), Rf_ScalarInteger(CGIPT));
      Rf_setAttrib(node, Rf_install("grads"), Rf_allocVector(VECSXP, 0));
      Rf_setAttrib(node, Rf_install("childeren"), Rf_allocVector(INTSXP, 0));
      break;

    case CGPRM :
      Rf_setAttrib(node, Rf_install("type"), Rf_ScalarInteger(CGPRM));
      Rf_setAttrib(node, Rf_install("grads"), Rf_allocVector(VECSXP, 0));
      Rf_setAttrib(node, Rf_install("childeren"), Rf_allocVector(INTSXP, 0));
      break;

    case CGOPR :
      Rf_setAttrib(node, Rf_install("type"), Rf_ScalarInteger(CGOPR));
      Rf_setAttrib(node, Rf_install("call"), R_NilValue);
      Rf_setAttrib(node, Rf_install("grads"), Rf_allocVector(VECSXP, 0));
      Rf_setAttrib(node, Rf_install("parents"), Rf_allocVector(INTSXP, 0));
      Rf_setAttrib(node, Rf_install("childeren"), Rf_allocVector(INTSXP, 0));
      break;

    default :
      Rf_errorcall(R_NilValue, "invalid type provided");
  }

  Rf_setAttrib(node, R_ClassSymbol, Rf_mkString("cg.node"));

  UNPROTECT(1);

  return node;
}

SEXP cg_add_constant(SEXP value, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, Rf_ScalarInteger(CGCST), graph));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(1);

  return node;
}

SEXP cg_add_input(SEXP value, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, Rf_ScalarInteger(CGIPT), graph));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(1);

  return node;
}

SEXP cg_add_parameter(SEXP value, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, Rf_ScalarInteger(CGPRM), graph));

  if(!Rf_isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(1);

  return node;
}

SEXP cg_get_parms(SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

  SEXP values = PROTECT(cg_find_values(graph));

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
    SEXP name = R_NilValue;

    SEXP value = VECTOR_ELT(parms, i);

    if(!Rf_isNull(names))
    {
      name = Rf_ScalarString(STRING_ELT(names, i));
    }

    cg_add_parameter(value, name, graph);
  }

  return R_NilValue;
}

SEXP cg_add_operation(SEXP call, SEXP grads, SEXP binding, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, Rf_ScalarInteger(CGOPR), graph));

  if(!(Rf_isLanguage(call) || Rf_isSymbol(call)))
  {
    Rf_errorcall(R_NilValue, "call must be a call or symbol");
  }

  if(TYPEOF(grads) != VECSXP)
  {
    Rf_errorcall(R_NilValue, "grads must be a named list");
  }

  if(!Rf_isEnvironment(binding))
  {
    Rf_errorcall(R_NilValue, "binding must be an environment");
  }

  SEXP vars = PROTECT(R_lsInternal3(binding, TRUE, FALSE));

  for(int i = 0; i < LENGTH(vars); i++)
  {
    SEXP var = STRING_ELT(vars, i);

    SEXP value = PROTECT(Rf_findVar(Rf_install(CHAR(var)), binding));

    if(!Rf_isString(value))
    {
      Rf_errorcall(R_NilValue, "object '%s' in binding must be a character scalar", CHAR(var));
    }

    Rf_setVar(Rf_install(CHAR(var)), Rf_coerceVector(value, SYMSXP), binding);

    UNPROTECT(1);
  }

  SEXP parents = R_NilValue;

  SEXP nodes = cg_find_nodes(graph);

  SEXP names = Rf_getAttrib(grads, R_NamesSymbol);

  if(Rf_isNull(names))
  {
    parents = PROTECT(Rf_allocVector(INTSXP, 0));
  }
  else
  {
    int g = LENGTH(names);

    parents = PROTECT(Rf_allocVector(INTSXP, g));

    for(int i = 0; i < g; i++)
    {
      SEXP grad_name = STRING_ELT(names, i);

      if(grad_name == R_BlankString)
      {
        Rf_errorcall(R_NilValue, "blank name provided for gradient at index %d", i + 1);
      }

      SEXP symbol = PROTECT(Rf_findVarInFrame(binding, Rf_install(CHAR(grad_name))));

      if(symbol == R_UnboundValue)
      {
        Rf_errorcall(R_NilValue, "cannot find object '%s' in binding", CHAR(STRING_ELT(names, i)));
      }

      INTEGER(parents)[i] = cg_node_id(symbol, graph);

      UNPROTECT(1);
    }

    int n = LENGTH(nodes);

    for(int i = 0; i < g; i++)
    {
      SEXP parent = VECTOR_ELT(nodes, INTEGER(parents)[i] - 1);


      // Add gradient to parent node
      SEXP parent_grads = Rf_getAttrib(parent, Rf_install("grads"));

      if(TYPEOF(parent_grads) != VECSXP)
      {
        Rf_errorcall(R_NilValue, "node '%s' has invalid grads", CHAR(Rf_asChar(parent)));
      }

      int h = LENGTH(parent_grads);

      parent_grads = PROTECT(Rf_lengthgets(parent_grads, h + 1));

      SET_VECTOR_ELT(parent_grads, h, Rf_substitute(VECTOR_ELT(grads, i), binding));

      Rf_setAttrib(parent, Rf_install("grads"), parent_grads);


      // Add node as child to parent node
      SEXP parent_childeren = Rf_getAttrib(parent, Rf_install("childeren"));

      if(!Rf_isInteger(parent_childeren))
      {
        Rf_errorcall(R_NilValue, "node '%s' has invalid childeren", CHAR(Rf_asChar(parent)));
      }

      int c = LENGTH(parent_childeren);

      parent_childeren = PROTECT(Rf_lengthgets(parent_childeren, c + 1));

      INTEGER(parent_childeren)[c] = n + 1;

      Rf_setAttrib(parent, Rf_install("childeren"), parent_childeren);


      UNPROTECT(2);
    }
  }

  Rf_setAttrib(node, Rf_install("call"), Rf_substitute(call, binding));
  Rf_setAttrib(node, Rf_install("parents"), parents);

  cg_add_node(node, graph);

  UNPROTECT(3);

  return node;
}

static SEXP cg_traverse_graph(SEXP name, SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

  int l = 0, n = LENGTH(nodes), visited[n];

  SEXP ids = PROTECT(Rf_allocVector(INTSXP, n));

  memset(visited, 0, n * sizeof(int));

  stack s = stack_init(n);

  stack_push(&s, cg_node_id(name, graph));

  while(!stack_is_empty(&s))
  {
    int current = stack_peek(&s);

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
            stack_push(&s, INTEGER(parents)[i]);
          }
        }
      }
      else
      {
        INTEGER(ids)[l] = stack_pop(&s);

        l++;
      }
    }
    else
    {
      if(visited[current - 1] == 1)
      {
        if(Rf_isInteger(parents))
        {
          INTEGER(ids)[l] = stack_pop(&s);

          l++;
        }
        else
        {
          stack_remove(&s);
        }
      }
      else
      {
        stack_remove(&s);
      }
    }

    visited[current - 1] += 1;
  }

  SETLENGTH(ids, l);

  stack_destroy(&s);

  UNPROTECT(2);

  return ids;
}

static void cg_forward(SEXP ids, SEXP values, SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

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
      Rf_errorcall(R_NilValue, "object '%s' does not evaluate to a numeric vector or array but a %s",
                CHAR(Rf_asChar(node)), Rf_type2char(TYPEOF(value)));
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
}

static void cg_backward(SEXP ids, SEXP index, SEXP values, SEXP grads, SEXP graph)
{
  int n = LENGTH(ids);

  SEXP nodes = PROTECT(cg_find_nodes(graph));

  SEXP child_grad_env = PROTECT(NewEnv(values));

  if(n > 0)
  {
    SEXP root = VECTOR_ELT(nodes, INTEGER(ids)[n - 1] - 1);

    if(!is_cg_node(root))
    {
      Rf_errorcall(R_NilValue, "node '%s' is not a valid cg.node object", CHAR(Rf_asChar(root)));
    }

    SEXP root_value = PROTECT(Rf_eval(Rf_install(CHAR(Rf_asChar(root))), values));

    SEXP root_grad = PROTECT(Rf_duplicate(root_value));

    if(!Rf_isNumber(root_grad))
    {
      Rf_errorcall(R_NilValue, "cannot differentiate an object of type %s", Rf_type2char(TYPEOF(root_grad)));
    }

    if(Rf_isInteger(root_grad))
    {
      root_grad = Rf_coerceVector(root_grad, REALSXP);
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

        SEXP node_grad = PROTECT(R_NilValue);

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

            SEXP current_grad = PROTECT(Rf_eval(node_grad_call, child_grad_env));

            if(!Rf_isNumeric(current_grad))
            {
              Rf_errorcall(R_NilValue, "the gradient of node '%s' at index %d does not evaluate to a numeric vector or array but a %s",
                           CHAR(Rf_asChar(node)), j + 1, Rf_type2char(TYPEOF(current_grad)));
            }

            if(Rf_isInteger(current_grad))
            {
              current_grad = Rf_coerceVector(current_grad, REALSXP);
            }

            if(Rf_isNull(node_grad))
            {
              node_grad = current_grad;
            }
            else
            {
              for(int k = 0; k < LENGTH(current_grad); k++)
              {
                REAL(node_grad)[k] += REAL(current_grad)[k];
              }
            }

            UNPROTECT(1);
          }

          UNPROTECT(1);
        }

        Rf_defineVar(Rf_install(CHAR(Rf_asChar(node))), node_grad, grads);

        UNPROTECT(1);
      }
    }

    UNPROTECT(2);
  }

  UNPROTECT(2);
}

SEXP cg_run(SEXP name, SEXP values, SEXP graph)
{
  if(!Rf_isString(name) || Rf_asChar(name) == R_BlankString)
  {
    Rf_errorcall(R_NilValue, "name must be a non-blank character scalar");
  }

  if(!Rf_isEnvironment(values))
  {
    Rf_errorcall(R_NilValue, "values must be an environment");
  }

  SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  SET_ENCLOS(values, cg_find_values(graph));

  cg_forward(ids, values, graph);

  UNPROTECT(1);

  return values;
}

SEXP cg_gradients(SEXP name, SEXP values, SEXP index, SEXP graph)
{
  SEXP grads = PROTECT(NewEnv(R_NilValue));

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

  SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  SET_ENCLOS(values, cg_find_values(graph));

  cg_backward(ids, index, values, grads, graph);

  UNPROTECT(2);

  return grads;
}

SEXP cg_approx_grad(SEXP x, SEXP y, SEXP values, SEXP index, SEXP eps, SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

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

  SEXP ids = PROTECT(cg_traverse_graph(x, graph));

  SEXP x_node = VECTOR_ELT(nodes, cg_node_id(x, graph) - 1);
  SEXP y_node = VECTOR_ELT(nodes, cg_node_id(y, graph) - 1);

  SEXP x_node_type = Rf_getAttrib(x_node, Rf_install("type"));
  SEXP y_node_type = Rf_getAttrib(y_node, Rf_install("type"));

  if(!Rf_isInteger(x_node_type))
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(Rf_asChar(x)));
  }

  if(!Rf_isInteger(y_node_type))
  {
    Rf_errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(Rf_asChar(y)));
  }

  if(Rf_asInteger(x_node_type) != CGOPR)
  {
    Rf_errorcall(R_NilValue, "x must be an operation node");
  }

  if(Rf_asInteger(y_node_type) == CGOPR)
  {
    Rf_errorcall(R_NilValue, "y cannot be an operation node");
  }

  SET_ENCLOS(values, cg_find_values(graph));

  cg_forward(ids, values, graph);

  SEXP x_value = PROTECT(Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values));
  SEXP y_value = PROTECT(Rf_eval(Rf_install(CHAR(Rf_asChar(y_node))), values));

  if(Rf_asInteger(index) < 1 || Rf_asInteger(index) > LENGTH(x_value))
  {
    Rf_errorcall(R_NilValue, "invalid index provided");
  }

  SEXP grad = PROTECT(Rf_duplicate(y_value));

  for(int i = 0; i < LENGTH(grad); i++)
  {
    REAL(y_value)[i] += Rf_asReal(eps);

    cg_forward(ids, values, graph);

    SEXP x_value1 = PROTECT(Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values));

    REAL(y_value)[i] -= 2 * Rf_asReal(eps);

    cg_forward(ids, values, graph);

    SEXP x_value2 = PROTECT(Rf_eval(Rf_install(CHAR(Rf_asChar(x_node))), values));

    REAL(grad)[i] = (REAL(x_value1)[Rf_asInteger(index) - 1] - REAL(x_value2)[Rf_asInteger(index) - 1]) / (2 * Rf_asReal(eps));

    REAL(y_value)[i] += Rf_asReal(eps);

    UNPROTECT(2);
  }

  UNPROTECT(5);

  return grad;
}

SEXP cg_adj_mat(SEXP graph)
{
  SEXP nodes = PROTECT(cg_find_nodes(graph));

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
