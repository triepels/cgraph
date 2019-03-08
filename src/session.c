/*
Copyright 2019 Ron Triepels

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

#include "class.h"

/*
 * SYMBOLS
 */

#define CG_SESSION_SYMBOL Rf_install("session")
#define CG_GRAPH_SYMBOL Rf_install("graph")

/*
 * PRIVATE METHODS
 */

SEXP cg_session_get()
{
  SEXP env = PROTECT(R_FindNamespace(Rf_mkString("cgraph")));

  SEXP session = PROTECT(Rf_eval(CG_SESSION_SYMBOL, env));

  if(!cg_is(session, "cg_session"))
  {
    Rf_errorcall(R_NilValue, "invalid session");
  }

  UNPROTECT(2);

  return session;
}

/*
 * PUBLIC METHODS
 */

SEXP cg_session_graph()
{
  SEXP session = PROTECT(cg_session_get());

  SEXP graph = Rf_findVarInFrame(session, CG_GRAPH_SYMBOL);

  if(graph == R_UnboundValue)
  {
    Rf_errorcall(R_NilValue, "no active graph has been set");
  }

  UNPROTECT(1);

  return graph;
}

SEXP cg_session_set_graph(SEXP graph)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  SEXP session = PROTECT(cg_session_get());

  Rf_defineVar(CG_GRAPH_SYMBOL, graph, session);

  UNPROTECT(1);

  return R_NilValue;
}

/*
 * PUBLIC CONSTRUCTORS
 */

SEXP cg_session()
{
  SEXP session = PROTECT(cg_class1("cg_session"));

  SEXP env = PROTECT(R_FindNamespace(Rf_mkString("cgraph")));

  Rf_defineVar(CG_SESSION_SYMBOL, session, env);

  UNPROTECT(2);

  return session;
}
