#include <R.h>
#include <Rinternals.h>

#include "stack.h"

#define CGCST 0
#define CGIPT 1
#define CGPRM 2
#define CGEXP 3

SEXP vec2Chr(SEXP x)
{
  return coerceVector(x, CHARSXP);
}

SEXP cgraph(SEXP graph, SEXP values, SEXP grad)
{
  SEXP nodes = PROTECT(allocVector(VECSXP, 0));

  defineVar(install("nodes"), nodes, graph);
  defineVar(install("values"), values, graph);
  defineVar(install("grad"), grad, graph);

  setAttrib(graph, install("class"), mkString("cgraph"));

  UNPROTECT(1);

  return graph;
}

SEXP cg_types()
{
  SEXP types = PROTECT(allocVector(STRSXP, 4));

  SET_STRING_ELT(types, 0, mkChar("constant"));
  SET_STRING_ELT(types, 1, mkChar("input"));
  SET_STRING_ELT(types, 2, mkChar("parameter"));
  SET_STRING_ELT(types, 3, mkChar("expression"));

  UNPROTECT(1);

  return types;
}

SEXP cg_gen_id(SEXP graph)
{
  SEXP nodes = findVar(install("nodes"), graph);

  return ScalarInteger(LENGTH(nodes) + 1);
}

int cg_node_id(SEXP name, SEXP graph)
{
  SEXP nodes = findVar(install("nodes"), graph);

  int n = LENGTH(nodes) - 1;

  for(int i = n; i >= 0; i--)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(CHAR(asChar(node)), CHAR(name)) == 0)
    {
      return i + 1;
    }
  }

  error("cannot find node '%s'", CHAR(name));
}

int cg_node_exists(SEXP name, SEXP graph)
{
  SEXP nodes = findVar(install("nodes"), graph);

  int n = LENGTH(nodes);

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(strcmp(CHAR(asChar(node)), CHAR(name)) == 0)
    {
      return TRUE;
    }
  }

  return FALSE;
}

SEXP cg_add_placeholder(SEXP value, SEXP name, SEXP type, SEXP graph)
{
  SEXP id = cg_gen_id(graph);

  SEXP symbol = coerceVector(name, SYMSXP);

  SEXP nodes = lengthgets(findVar(install("nodes"), graph), asInteger(id));

  if(cg_node_exists(asChar(name), graph))
  {
    error("'%s' is already defined", CHAR(asChar(name)));
  }

  if(strcmp(CHAR(asChar(name)), "grad") == 0)
  {
    error("'grad' is a reserved word that cannot be used");
  }

  setAttrib(name, install("id"), id);

  switch(asInteger(type))
  {
    case CGCST :
      setAttrib(name, install("type"), ScalarInteger(CGCST)); break;
    case CGIPT :
      setAttrib(name, install("type"), ScalarInteger(CGIPT)); break;
    case CGPRM :
      setAttrib(name, install("type"), ScalarInteger(CGPRM)); break;
    default :
      error("invalid type");
  }

  setAttrib(name, install("grads"), allocVector(VECSXP, 0));
  setAttrib(name, install("childeren"), allocVector(INTSXP, 0));
  setAttrib(name, install("class"), mkString("cg.node"));

  SET_VECTOR_ELT(nodes, asInteger(id) - 1, name);

  setVar(install("nodes"), nodes, graph);

  if(!isNull(value))
  {
    defineVar(symbol, value, findVar(install("values"), graph));
  }

  return symbol;
}

SEXP cg_add_expression(SEXP call, SEXP grads, SEXP binding, SEXP name, SEXP graph)
{
  int n = LENGTH(grads);

  SEXP id = cg_gen_id(graph);

  SEXP symbol = coerceVector(name, SYMSXP);

  SEXP nodes = lengthgets(findVar(install("nodes"), graph), asInteger(id));

  SEXP names = getAttrib(grads, R_NamesSymbol);

  SEXP parents = PROTECT(allocVector(INTSXP, n));

  if(cg_node_exists(asChar(name), graph))
  {
    error("'%s' is already defined", CHAR(asChar(name)));
  }

  if(strcmp(CHAR(asChar(name)), "grad") == 0)
  {
    error("'grad' is a reserved word that cannot be used");
  }

  for(int i = 0; i < n; i++)
  {
    SEXP parent_symbol = findVarInFrame(binding, install(CHAR(STRING_ELT(names, i))));

    if(parent_symbol == R_UnboundValue)
    {
      error("cannot find symbol '%s' in binding", CHAR(STRING_ELT(names, i)));
    }

    parent_symbol = coerceVector(parent_symbol, SYMSXP);

    setVar(install(CHAR(STRING_ELT(names, i))), parent_symbol, binding);
  }

  for(int i = 0; i < n; i++)
  {
    int m;

    SEXP parent_symbol = findVar(install(CHAR(STRING_ELT(names, i))), binding);

    SEXP parent = VECTOR_ELT(nodes, cg_node_id(vec2Chr(parent_symbol), graph) - 1);


    /* Add gradient to parent node */
    SEXP parent_grads = getAttrib(parent, install("grads"));

    m = LENGTH(parent_grads) + 1;

    parent_grads = lengthgets(parent_grads, m);

    SET_VECTOR_ELT(parent_grads, m - 1, substitute(VECTOR_ELT(grads, i), binding));

    setAttrib(parent, install("grads"), parent_grads);


    /* Add current id to parent node */
    SEXP parent_childeren = getAttrib(parent, install("childeren"));

    m = LENGTH(parent_childeren) + 1;

    parent_childeren = lengthgets(parent_childeren, m);

    INTEGER(parent_childeren)[m - 1] = asInteger(id);

    setAttrib(parent, install("childeren"), parent_childeren);


    /* Add parent id to the current node */
    INTEGER(parents)[i] = asInteger(getAttrib(parent, install("id")));
  }

  setAttrib(name, install("id"), id);
  setAttrib(name, install("type"), ScalarInteger(CGEXP));
  setAttrib(name, install("call"), substitute(call, binding));
  setAttrib(name, install("grads"), allocVector(VECSXP, 0));
  setAttrib(name, install("parents"), parents);
  setAttrib(name, install("childeren"), allocVector(INTSXP, 0));
  setAttrib(name, install("class"), mkString("cg.node"));

  SET_VECTOR_ELT(nodes, asInteger(id) - 1, name);

  setVar(install("nodes"), nodes, graph);

  UNPROTECT(1);

  return symbol;
}

SEXP cg_count_type(SEXP type, SEXP graph)
{
  SEXP nodes = findVar(install("nodes"), graph);

  int total = 0; int n = LENGTH(nodes);

  for (int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    if(asInteger(getAttrib(node, install("type"))) == asInteger(type))
    {
      total += 1;
    }
  }

  return ScalarInteger(total);
}

SEXP cg_gen_name(SEXP type, SEXP graph)
{
  char name[4];

  SEXP count = PROTECT(cg_count_type(type, graph));

  INTEGER(count)[0] += 1;

  switch(asInteger(type))
  {
    case CGCST :
      sprintf(name, "cst%d", asInteger(count)); break;
    case CGIPT :
      sprintf(name, "ipt%d", asInteger(count)); break;
    case CGPRM :
      sprintf(name, "prm%d", asInteger(count)); break;
    case CGEXP :
      sprintf(name, "exp%d", asInteger(count)); break;
    default :
      error("invalid type");
  }

  UNPROTECT(1);

  return coerceVector(mkString(name), SYMSXP);
}

SEXP cg_traverse_graph(SEXP id, SEXP graph)
{
  SEXP nodes = findVar(install("nodes"), graph);

  int n = LENGTH(nodes), k = 0; int score[n]; stack s;

  SEXP ids = PROTECT(allocVector(INTSXP, n));

  memset(score, 0, n * sizeof(int));

  stack_init(&s, n);

  stack_push(&s, asInteger(id));

  while(!stack_is_empty(&s))
  {
    int current = stack_peek(&s);

    if(score[current - 1] > 1)
    {
      stack_remove(&s);
    }
    else
    {
      SEXP node = VECTOR_ELT(nodes, current - 1);

      SEXP parents = getAttrib(node, install("parents"));

      if(score[current - 1] > 0 || isNull(parents))
      {
        stack_remove(&s);

        INTEGER(ids)[k] = current;

        score[current - 1]++;

        k++;
      }
      else
      {
        int m = LENGTH(parents);

        for(int j = 0; j < m; j++)
        {
          if(score[INTEGER(parents)[j] - 1] == 0)
          {
            stack_push(&s, INTEGER(parents)[j]);
          }
        }
      }

      score[current - 1]++;
    }
  }

  SETLENGTH(ids, k);

  stack_destroy(&s);

  UNPROTECT(1);

  return ids;
}

void cg_forward(SEXP ids, SEXP values, SEXP graph)
{
  int n = LENGTH(ids);

  SEXP nodes = findVar(install("nodes"), graph);

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, INTEGER(ids)[i] - 1);

    SEXP type = getAttrib(node, install("type"));

    if(asInteger(type) == CGEXP)
    {
      SEXP symbol = coerceVector(node, SYMSXP);

      SEXP call = getAttrib(node, install("call"));

      SEXP value = eval(call, values);

      SEXP dim = getAttrib(value, R_DimSymbol);

      if(isNull(dim))
      {
        if(strcmp(CHAR(vec2Chr(CAR(call))), "c") != 0)
        {
          dim = PROTECT(allocVector(INTSXP, 1));

          INTEGER(dim)[0] = LENGTH(value);

          setAttrib(value, R_DimSymbol, dim);

          UNPROTECT(1);
        }
      }

      defineVar(symbol, value, values);
    }
  }
}

void cg_backward(SEXP ids, SEXP index, SEXP values, SEXP grads, SEXP graph)
{
  int n = LENGTH(ids);

  SEXP nodes = findVar(install("nodes"), graph);

  SEXP grad_env = findVar(install("grad"), graph);

  if(n > 0)
  {
    SEXP root = VECTOR_ELT(nodes, INTEGER(ids)[n - 1] - 1);

    SEXP root_value = eval(install(CHAR(asChar(root))), values);

    SEXP root_grad = coerceVector(duplicate(root_value), INTSXP);

    int m = LENGTH(root_grad);

    if(INTEGER(index)[0] < 1 | INTEGER(index)[0] > m)
    {
      error("invalid index");
    }

    memset(INTEGER(root_grad), 0, m * sizeof(int));

    INTEGER(root_grad)[INTEGER(index)[0] - 1] = 1;

    defineVar(install(CHAR(asChar(root))), root_grad, grads);

    for(int i = n - 2; i >= 0; i--)
    {
      SEXP node = VECTOR_ELT(nodes, INTEGER(ids)[i] - 1);

      SEXP type = getAttrib(node, install("type"));

      if(asInteger(type) == CGPRM || asInteger(type) == CGEXP)
      {
        SEXP node_grads = getAttrib(node, install("grads"));

        SEXP node_childeren = getAttrib(node, install("childeren"));

        if(!isNull(node_grads) & !isNull(node_childeren))
        {
          int assign = 1, m = LENGTH(node_childeren);

          SEXP node_grad = R_NilValue;

          for(int j = 0; j < m; j++)
          {
            SEXP child = VECTOR_ELT(nodes, INTEGER(node_childeren)[j] - 1);

            SEXP child_value = findVarInFrame(grads, install(CHAR(asChar(child))));

            if(child_value != R_UnboundValue)
            {
              defineVar(install("grad"), child_value, grad_env);

              if(assign)
              {
                node_grad = eval(VECTOR_ELT(node_grads, j), values);

                assign = 0;
              }
              else
              {
                SEXP grad = eval(VECTOR_ELT(node_grads, j), values);

                int l = LENGTH(grad);

                for(int k = 0; k < l; k++)
                {
                  REAL(node_grad)[k] += REAL(grad)[k];
                }
              }
            }
          }

          defineVar(install(CHAR(asChar(node))), node_grad, grads);
        }
      }
    }
  }
}

SEXP cg_run(SEXP name, SEXP values, SEXP graph)
{
  int id = cg_node_id(asChar(name), graph);

  SEXP ids = cg_traverse_graph(ScalarInteger(id), graph);

  cg_forward(ids, values, graph);

  setAttrib(values, install("class"), mkString("cg.results"));

  return values;
}

SEXP cg_gradients(SEXP name, SEXP index, SEXP values, SEXP grads, SEXP graph)
{
  int id = cg_node_id(asChar(name), graph);

  SEXP ids = cg_traverse_graph(ScalarInteger(id), graph);

  cg_backward(ids, index, values, grads, graph);

  setAttrib(grads, install("class"), mkString("cg.results"));

  return grads;
}

SEXP cg_adj_mat(SEXP graph)
{
  SEXP nodes = findVar(install("nodes"), graph);

  int n = LENGTH(nodes);

  SEXP names = PROTECT(allocVector(STRSXP, n));

  SEXP mat = PROTECT(allocMatrix(INTSXP, n, n));

  memset(INTEGER(mat), 0, n * n * sizeof(int));

  for(int i = 0; i < n; i++)
  {
    SEXP node = VECTOR_ELT(nodes, i);

    SEXP childeren = getAttrib(node, install("childeren"));

    if(!isNull(childeren))
    {
      int m = LENGTH(childeren);

      for (int j = 0; j < m; j++)
      {
        INTEGER(mat)[i + n * (INTEGER(childeren)[j] - 1)] = 1;
      }
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
