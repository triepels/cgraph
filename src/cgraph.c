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

#define CGCST 0
#define CGIPT 1
#define CGPRM 2
#define CGOPR 3

static SEXP NewEnv(SEXP enclos)
{
  SEXP env = PROTECT(allocSExp(ENVSXP));

  SET_FRAME(env, R_NilValue);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);

  if(isNull(enclos) || !isEnvironment(enclos))
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
  SEXP nodes = PROTECT(allocVector(VECSXP, 0));

  if(isNull(graph) || !isEnvironment(graph))
  {
    graph = NewEnv(R_EmptyEnv);
  }

  if(isNull(values) || !isEnvironment(values))
  {
    values = NewEnv(R_BaseEnv);
  }

  setAttrib(nodes, R_NamesSymbol, allocVector(STRSXP, 0));

  defineVar(install("nodes"), nodes, graph);
  defineVar(install("values"), values, graph);

  setAttrib(graph, install("class"), mkString("cgraph"));

  UNPROTECT(1);

  return graph;
}

int is_cgraph(SEXP x)
{
  SEXP x_class = getAttrib(x, R_ClassSymbol);

  if(!isEnvironment(x))
  {
    return FALSE;
  }

  if(!isString(x_class))
  {
    return FALSE;
  }

  if(strcmp(CHAR(asChar(x_class)), "cgraph") != 0)
  {
    return FALSE;
  }

  return TRUE;
}

int is_cg_node(SEXP x)
{
  SEXP x_class = getAttrib(x, R_ClassSymbol);

  if(!isString(x) || asChar(x) == R_BlankString)
  {
    return FALSE;
  }

  if(!isString(x_class))
  {
    return FALSE;
  }

  if(strcmp(CHAR(asChar(x_class)), "cg.node") != 0)
  {
    return FALSE;
  }

  return TRUE;
}

SEXP cg_find_nodes(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP nodes = findVarInFrame(graph, install("nodes"));

  SEXP names = getAttrib(nodes, R_NamesSymbol);

  if(TYPEOF(nodes) != VECSXP || !isString(names))
  {
    errorcall(R_NilValue, "cannot find valid nodes object in cgraph object environment");
  }

  return nodes;
}

SEXP cg_find_values(SEXP graph)
{
  if(!is_cgraph(graph))
  {
    errorcall(R_NilValue, "invalid cgraph object provided");
  }

  SEXP values = findVarInFrame(graph, install("values"));

  if(!isEnvironment(values))
  {
    errorcall(R_NilValue, "cannot find valid values object in cgraph object environment");
  }

  return values;
}

void cg_add_node(SEXP node, SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  SEXP names = getAttrib(nodes, R_NamesSymbol);

  if(!is_cg_node(node))
  {
    errorcall(R_NilValue, "invalid cg.node object provided");
  }

  int n = LENGTH(nodes);

  nodes = PROTECT(lengthgets(nodes, n + 1));

  names = PROTECT(lengthgets(names, n + 1));

  SET_VECTOR_ELT(nodes, n, node);

  SET_STRING_ELT(names, n, asChar(node));

  setAttrib(nodes, R_NamesSymbol, names);

  setVar(install("nodes"), nodes, graph);

  UNPROTECT(2);
}

void cg_add_value(SEXP node, SEXP value, SEXP graph)
{
  SEXP values = cg_find_values(graph);

  if(!is_cg_node(node))
  {
    errorcall(R_NilValue, "invalid cg.node object provided");
  }

  if(!isNumeric(value))
  {
    errorcall(R_NilValue, "node '%s' does not evaluate to a numeric vector or array", CHAR(asChar(node)));
  }

  if(isInteger(value))
  {
    value = coerceVector(value, REALSXP);
  }

  defineVar(install(CHAR(asChar(node))), value, values);
}

SEXP cg_gen_name(SEXP type, SEXP graph)
{
  char name[32];

  int count = 0;

  SEXP nodes = cg_find_nodes(graph);

  if(!isNumber(type))
  {
    errorcall(R_NilValue, "type must be a numeric scalar");
  }

  for(int i = 0; i < LENGTH(nodes); i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    SEXP node_type = getAttrib(node, install("type"));

    if(!isInteger(node_type))
    {
      errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(asChar(node)));
    }

    if(asInteger(node_type) == asInteger(type))
    {
      count += 1;
    }
  }

  switch(asInteger(type))
  {
    case CGCST :
      sprintf(name, "cst%d", count + 1);
      break;

    case CGIPT :
      sprintf(name, "ipt%d", count + 1);
      break;

    case CGPRM :
      sprintf(name, "prm%d", count + 1);
      break;

    case CGOPR :
      sprintf(name, "opr%d", count + 1);
      break;

    default :
      errorcall(R_NilValue, "invalid type provided");
  }

  return mkString(name);
}

int cg_node_id(SEXP name, SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  if(!(isString(name) || isSymbol(name)))
  {
    errorcall(R_NilValue, "name must be a character scalar or symbol");
  }

  for(int i = LENGTH(nodes) - 1; i >= 0; i--)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(CHAR(asChar(node)), CHAR(asChar(name))) == 0)
    {
      return i + 1;
    }
  }

  errorcall(R_NilValue, "cannot find node '%s'", CHAR(asChar(name)));
}

int cg_node_exists(SEXP name, SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  if(!(isString(name) || isSymbol(name)))
  {
    errorcall(R_NilValue, "name must be a character scalar or symbol");
  }

  for(int i = 0; i < LENGTH(nodes); i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(CHAR(asChar(node)), CHAR(asChar(name))) == 0)
    {
      return TRUE;
    }
  }

  return FALSE;
}

SEXP cg_node(SEXP name, SEXP type, SEXP graph)
{
  SEXP node = R_NilValue;

  if(isNull(name))
  {
    node = PROTECT(cg_gen_name(type, graph));
  }
  else
  {
    if(!isString(name) || asChar(name) == R_BlankString)
    {
      errorcall(R_NilValue, "the name of a node must be a non-blank character scalar");
    }

    node = PROTECT(mkString(CHAR(asChar(name))));
  }

  if(cg_node_exists(node, graph))
  {
    errorcall(R_NilValue, "node '%s' is already defined", CHAR(asChar(node)));
  }

  if(strcmp(CHAR(asChar(node)), "grad") == 0)
  {
    errorcall(R_NilValue, "'grad' is a reserved word that cannot be used");
  }

  if(!isNumber(type))
  {
    errorcall(R_NilValue, "type must be an numeric scalar");
  }

  switch(asInteger(type))
  {
    case CGCST :
      setAttrib(node, install("type"), ScalarInteger(CGCST));
      setAttrib(node, install("grads"), allocVector(VECSXP, 0));
      setAttrib(node, install("childeren"), allocVector(INTSXP, 0));
      break;

    case CGIPT :
      setAttrib(node, install("type"), ScalarInteger(CGIPT));
      setAttrib(node, install("grads"), allocVector(VECSXP, 0));
      setAttrib(node, install("childeren"), allocVector(INTSXP, 0));
      break;

    case CGPRM :
      setAttrib(node, install("type"), ScalarInteger(CGPRM));
      setAttrib(node, install("grads"), allocVector(VECSXP, 0));
      setAttrib(node, install("childeren"), allocVector(INTSXP, 0));
      break;

    case CGOPR :
      setAttrib(node, install("type"), ScalarInteger(CGOPR));
      setAttrib(node, install("call"), R_NilValue);
      setAttrib(node, install("grads"), allocVector(VECSXP, 0));
      setAttrib(node, install("parents"), allocVector(INTSXP, 0));
      setAttrib(node, install("childeren"), allocVector(INTSXP, 0));
      break;

    default :
      errorcall(R_NilValue, "invalid type provided");
  }

  setAttrib(node, R_ClassSymbol, mkString("cg.node"));

  UNPROTECT(1);

  return node;
}

SEXP cg_add_constant(SEXP value, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, ScalarInteger(CGCST), graph));

  if(!isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(1);

  return node;
}

SEXP cg_add_input(SEXP value, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, ScalarInteger(CGIPT), graph));

  if(!isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(1);

  return node;
}

SEXP cg_add_parameter(SEXP value, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, ScalarInteger(CGPRM), graph));

  if(!isNull(value))
  {
    cg_add_value(node, value, graph);
  }

  cg_add_node(node, graph);

  UNPROTECT(1);

  return node;
}

SEXP cg_get_parms(SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  SEXP values = cg_find_values(graph);

  int n = LENGTH(nodes), l = 0;

  SEXP parms = PROTECT(allocVector(VECSXP, n));

  SEXP names = PROTECT(allocVector(STRSXP, n));

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    SEXP node_type = getAttrib(node, install("type"));

    if(!isInteger(node_type))
    {
      errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(asChar(node)));
    }

    if(asInteger(node_type) == CGPRM)
    {
      SEXP value = findVarInFrame(values, install(CHAR(asChar(node))));

      if(value != R_UnboundValue)
      {
        SET_VECTOR_ELT(parms, l, value);
      }

      SET_STRING_ELT(names, l, asChar(node));

      l++;
    }
  }

  SETLENGTH(parms, l);
  SETLENGTH(names, l);

  setAttrib(parms, R_NamesSymbol, names);

  UNPROTECT(2);

  return(parms);
}

SEXP cg_add_parms(SEXP parms, SEXP graph)
{
  SEXP names = getAttrib(parms, R_NamesSymbol);

  if(TYPEOF(parms) != VECSXP)
  {
    errorcall(R_NilValue, "parms must be a named list");
  }

  for(int i = 0; i < LENGTH(parms); i++)
  {
    SEXP name = R_NilValue;

    SEXP value = VECTOR_ELT(parms, i);

    if(!isNull(names))
    {
      name = ScalarString(STRING_ELT(names, i));
    }

    cg_add_parameter(value, name, graph);
  }

  return R_NilValue;
}

SEXP cg_add_operation(SEXP call, SEXP grads, SEXP binding, SEXP name, SEXP graph)
{
  SEXP node = PROTECT(cg_node(name, ScalarInteger(CGOPR), graph));

  if(!(isLanguage(call) || isSymbol(call)))
  {
    errorcall(R_NilValue, "call must be a call or symbol");
  }

  if(TYPEOF(grads) != VECSXP)
  {
    errorcall(R_NilValue, "grads must be a named list");
  }

  if(!isEnvironment(binding))
  {
    errorcall(R_NilValue, "binding must be an environment");
  }

  SEXP vars = R_lsInternal3(binding, TRUE, FALSE);

  for(int i = 0; i < LENGTH(vars); i++)
  {
    SEXP var = STRING_ELT(vars, i);

    SEXP value = findVar(install(CHAR(var)), binding);

    if(!isString(value))
    {
      errorcall(R_NilValue, "object '%s' in binding must be a character scalar", CHAR(var));
    }

    setVar(install(CHAR(var)), coerceVector(value, SYMSXP), binding);
  }

  SEXP parents = R_NilValue;

  SEXP nodes = cg_find_nodes(graph);

  SEXP names = getAttrib(grads, R_NamesSymbol);

  if(isNull(names))
  {
    parents = PROTECT(allocVector(INTSXP, 0));
  }
  else
  {
    int n = LENGTH(nodes), g = LENGTH(names);

    parents = PROTECT(allocVector(INTSXP, g));

    for(int i = 0; i < g; i++)
    {
      SEXP grad_name = STRING_ELT(names, i);

      if(grad_name == R_BlankString)
      {
        errorcall(R_NilValue, "blank name provided for gradient at index %d", i + 1);
      }

      SEXP symbol = findVarInFrame(binding, install(CHAR(grad_name)));

      if(symbol == R_UnboundValue)
      {
        errorcall(R_NilValue, "cannot find object '%s' in binding", CHAR(STRING_ELT(names, i)));
      }

      INTEGER(parents)[i] = cg_node_id(symbol, graph);
    }

    for(int i = 0; i < g; i++)
    {
      SEXP parent = VECTOR_ELT(nodes, INTEGER(parents)[i] - 1);


      /* Add gradient to parent node */
      SEXP parent_grads = getAttrib(parent, install("grads"));

      if(TYPEOF(parent_grads) != VECSXP)
      {
        errorcall(R_NilValue, "node '%s' has invalid grads", CHAR(asChar(node)));
      }

      int h = LENGTH(parent_grads);

      parent_grads = PROTECT(lengthgets(parent_grads, h + 1));

      SET_VECTOR_ELT(parent_grads, h, substitute(VECTOR_ELT(grads, i), binding));

      setAttrib(parent, install("grads"), parent_grads);


      /* Add child to parent node */
      SEXP parent_childeren = getAttrib(parent, install("childeren"));

      if(!isInteger(parent_childeren))
      {
        errorcall(R_NilValue, "node '%s' has invalid childeren", CHAR(asChar(node)));
      }

      int c = LENGTH(parent_childeren);

      parent_childeren = PROTECT(lengthgets(parent_childeren, c + 1));

      INTEGER(parent_childeren)[c] = n + 1;

      setAttrib(parent, install("childeren"), parent_childeren);


      UNPROTECT(2);
    }
  }

  setAttrib(node, install("call"), substitute(call, binding));
  setAttrib(node, install("parents"), parents);

  cg_add_node(node, graph);

  UNPROTECT(2);

  return node;
}

static SEXP cg_traverse_graph(SEXP name, SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  int l = 0, n = LENGTH(nodes), visited[n];

  SEXP ids = PROTECT(allocVector(INTSXP, n));

  memset(visited, 0, n * sizeof(int));

  stack s = stack_init(n);

  stack_push(&s, cg_node_id(name, graph));

  while(!stack_is_empty(&s))
  {
    int current = stack_peek(&s);

    SEXP node = VECTOR_ELT(nodes, current - 1);

    SEXP parents = getAttrib(node, install("parents"));

    if(visited[current - 1] == 0)
    {
      if(isInteger(parents))
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
        if(isInteger(parents))
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

  UNPROTECT(1);

  return ids;
}

static void cg_forward(SEXP ids, SEXP values, SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  for(int i = 0; i < LENGTH(ids); i++)
  {
    SEXP node = VECTOR_ELT(nodes, INTEGER(ids)[i] - 1);

    SEXP node_type = getAttrib(node, install("type"));

    if(!isInteger(node_type))
    {
      errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(asChar(node)));
    }

    SEXP value = R_NilValue;

    if(asInteger(node_type) == CGOPR)
    {
      SEXP call = getAttrib(node, install("call"));

      if(!(isLanguage(call) || isSymbol(call)))
      {
        errorcall(R_NilValue, "node '%s' has an invalid call", CHAR(asChar(node)));
      }

      value = PROTECT(eval(call, values));

      defineVar(install(CHAR(asChar(node))), value, values);

      UNPROTECT(1);
    }
    else
    {
      value = eval(install(CHAR(asChar(node))), values);
    }

    if(!isNumeric(value))
    {
      errorcall(R_NilValue, "object '%s' does not evaluate to a numeric vector or array but a %s",
                CHAR(asChar(node)), type2char(TYPEOF(value)));
    }
  }
}

static SEXP cg_root_grad(SEXP value, SEXP index)
{
  SEXP grad = PROTECT(duplicate(value));

  if(!isNumber(grad))
  {
    errorcall(R_NilValue, "cannot differentiate an object of type %s", type2char(TYPEOF(grad)));
  }

  int n = LENGTH(value);

  if(asInteger(index) < 1 || asInteger(index) > n)
  {
    errorcall(R_NilValue, "invalid index provided");
  }

  grad = coerceVector(grad, REALSXP);

  memset(REAL(grad), 0, n * sizeof(double));

  REAL(grad)[asInteger(index) - 1] = 1;

  UNPROTECT(1);

  return grad;
}

static void cg_backward(SEXP ids, SEXP index, SEXP values, SEXP grads, SEXP graph)
{
  int n = LENGTH(ids);

  SEXP nodes = cg_find_nodes(graph);

  SEXP grad_env = PROTECT(NewEnv(values));

  if(n > 0)
  {
    SEXP root = VECTOR_ELT(nodes, INTEGER(ids)[n - 1] - 1);

    SEXP root_value = eval(install(CHAR(asChar(root))), values);

    SEXP root_grad = PROTECT(cg_root_grad(root_value, index));

    defineVar(install(CHAR(asChar(root))), root_grad, grads);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = VECTOR_ELT(nodes, INTEGER(ids)[i] - 1);

      SEXP node_type = getAttrib(node, install("type"));

      if(!isInteger(node_type))
      {
        errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(asChar(node)));
      }

      if(asInteger(node_type) == CGPRM || asInteger(node_type) == CGOPR)
      {
        SEXP node_grads = getAttrib(node, install("grads"));

        SEXP node_childeren = getAttrib(node, install("childeren"));

        if(TYPEOF(node_grads) != VECSXP)
        {
          errorcall(R_NilValue, "node '%s' has invalid gradients", CHAR(asChar(node)));
        }

        if(!isInteger(node_childeren))
        {
          errorcall(R_NilValue, "node '%s' has invalid childeren", CHAR(asChar(node)));
        }

        if(LENGTH(node_grads) != LENGTH(node_childeren))
        {
          errorcall(R_NilValue, "node '%s' has an unequal number of gradients (%d) and childeren (%d)",
                    CHAR(asChar(node)), LENGTH(node_grads), LENGTH(node_childeren));
        }

        SEXP node_grad = R_NilValue;

        for(int j = 0; j < LENGTH(node_childeren); j++)
        {
          SEXP child = VECTOR_ELT(nodes, INTEGER(node_childeren)[j] - 1);

          SEXP child_value = findVarInFrame(grads, install(CHAR(asChar(child))));

          if(child_value != R_UnboundValue)
          {
            SEXP node_grad_call = VECTOR_ELT(node_grads, j);

            if(!(isLanguage(node_grad_call) || isSymbol(node_grad_call)))
            {
              errorcall(R_NilValue, "node '%s' has an invalid gradient at index %d", CHAR(asChar(node)), j + 1);
            }

            defineVar(install("grad"), child_value, grad_env);

            if(isNull(node_grad))
            {
              node_grad = eval(node_grad_call, grad_env);

              node_grad = PROTECT(duplicate(node_grad));
            }
            else
            {
              SEXP child_grad = PROTECT(eval(node_grad_call, grad_env));

              for(int k = 0; k < LENGTH(child_grad); k++)
              {
                REAL(node_grad)[k] += REAL(child_grad)[k];
              }

              UNPROTECT(1);
            }
          }
        }

        defineVar(install(CHAR(asChar(node))), node_grad, grads);

        if(!isNull(node_grad))
        {
          UNPROTECT(1);
        }
      }
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
}

SEXP cg_run(SEXP name, SEXP values, SEXP graph)
{
  if(!isString(name) || asChar(name) == R_BlankString)
  {
    errorcall(R_NilValue, "name must be a non-blank character scalar");
  }

  if(!isEnvironment(values))
  {
    errorcall(R_NilValue, "values must be an environment");
  }

  SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  SET_ENCLOS(values, cg_find_values(graph));

  cg_forward(ids, values, graph);

  UNPROTECT(1);

  return values;
}

SEXP cg_gradients(SEXP name, SEXP values, SEXP index, SEXP graph)
{
  if(!isString(name) || asChar(name) == R_BlankString)
  {
    errorcall(R_NilValue, "name must be a non-blank character scalar");
  }

  if(!isNumber(index))
  {
    errorcall(R_NilValue, "index must be a numeric scalar");
  }

  if(!isEnvironment(values))
  {
    errorcall(R_NilValue, "values must be an environment");
  }

  SEXP grads = PROTECT(NewEnv(R_NilValue));

  SEXP ids = PROTECT(cg_traverse_graph(name, graph));

  SET_ENCLOS(values, cg_find_values(graph));

  cg_backward(ids, index, values, grads, graph);

  UNPROTECT(2);

  return grads;
}

SEXP cg_approx_grad(SEXP x, SEXP y, SEXP values, SEXP index, SEXP eps, SEXP graph)
{
  int indx;

  double epsx;

  SEXP nodes = cg_find_nodes(graph);

  if(!isString(x) || asChar(x) == R_BlankString)
  {
    errorcall(R_NilValue, "x must be a non-blank character scalar");
  }

  if(!isString(y) || asChar(y) == R_BlankString)
  {
    errorcall(R_NilValue, "y must be a non-blank character scalar");
  }

  if(!isEnvironment(values))
  {
    errorcall(R_NilValue, "values must be an environment");
  }

  if(!isNumber(index))
  {
    errorcall(R_NilValue, "index must be a numeric scalar");
  }

  if(!isNumber(eps))
  {
    errorcall(R_NilValue, "eps must be a numeric scalar");
  }

  indx = asInteger(index);
  epsx = asReal(eps);

  SEXP ids = PROTECT(cg_traverse_graph(x, graph));

  SEXP x_node = VECTOR_ELT(nodes, cg_node_id(x, graph) - 1);
  SEXP y_node = VECTOR_ELT(nodes, cg_node_id(y, graph) - 1);

  SEXP x_node_type = getAttrib(x_node, install("type"));
  SEXP y_node_type = getAttrib(y_node, install("type"));

  if(!isInteger(x_node_type))
  {
    errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(asChar(x)));
  }

  if(!isInteger(y_node_type))
  {
    errorcall(R_NilValue, "node '%s' has an invalid type", CHAR(asChar(y)));
  }

  if(asInteger(x_node_type) != CGOPR)
  {
    errorcall(R_NilValue, "x must be an operation node");
  }

  if(asInteger(y_node_type) == CGOPR)
  {
    errorcall(R_NilValue, "y cannot be an operation node");
  }

  SET_ENCLOS(values, cg_find_values(graph));

  cg_forward(ids, values, graph);

  SEXP x_value = PROTECT(eval(install(CHAR(asChar(x_node))), values));
  SEXP y_value = PROTECT(eval(install(CHAR(asChar(y_node))), values));

  if(indx < 1 || indx > LENGTH(x_value))
  {
    errorcall(R_NilValue, "invalid index provided");
  }

  SEXP grad = PROTECT(duplicate(y_value));

  for(int i = 0; i < LENGTH(grad); i++)
  {
    REAL(y_value)[i] += epsx;

    cg_forward(ids, values, graph);

    SEXP x_value1 = PROTECT(eval(install(CHAR(asChar(x_node))), values));

    REAL(y_value)[i] -= 2 * epsx;

    cg_forward(ids, values, graph);

    SEXP x_value2 = PROTECT(eval(install(CHAR(asChar(x_node))), values));

    REAL(grad)[i] = (REAL(x_value1)[indx - 1] - REAL(x_value2)[indx - 1]) / (2 * epsx);

    REAL(y_value)[i] += epsx;

    UNPROTECT(2);
  }

  UNPROTECT(4);

  return grad;
}

SEXP cg_adj_mat(SEXP graph)
{
  SEXP nodes = cg_find_nodes(graph);

  int n = LENGTH(nodes);

  SEXP names = PROTECT(allocVector(STRSXP, n));

  SEXP mat = PROTECT(allocMatrix(INTSXP, n, n));

  memset(INTEGER(mat), 0, n * n * sizeof(int));

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    SEXP childeren = getAttrib(node, install("childeren"));

    if(!isInteger(childeren))
    {
      errorcall(R_NilValue, "node '%s' has invalid childeren", CHAR(asChar(node)));
    }

    for(int j = 0; j < LENGTH(childeren); j++)
    {
      INTEGER(mat)[i + n * (INTEGER(childeren)[j] - 1)] = 1;
    }

    SET_STRING_ELT(names, i, asChar(node));
  }

  SEXP dimnames = PROTECT(allocVector(VECSXP, 2));

  SET_VECTOR_ELT(dimnames, 0, names);
  SET_VECTOR_ELT(dimnames, 1, names);

  setAttrib(mat, R_DimNamesSymbol, dimnames);

  UNPROTECT(3);

  return mat;
}
