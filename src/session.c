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

#include "class.h"
#include "session.h"

/*
 * PUBLIC FUNCTIONS
 */

SEXP cg_session_graph()
{
  if(session.graph == NULL)
  {
    Rf_errorcall(R_NilValue, "no active graph has been set");
  }

  return session.graph;
}

SEXP cg_session_set_graph(SEXP graph)
{
  if(!cg_is(graph, "cg_graph"))
  {
    Rf_errorcall(R_NilValue, "argument 'graph' must be a cg_graph object");
  }

  session.graph = graph;

  return R_NilValue;
}
