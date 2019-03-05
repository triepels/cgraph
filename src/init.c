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
#include <R_ext/Rdynload.h>

#include "math.h"
#include "class.h"
#include "graph.h"
#include "function.h"
#include "internal.h"

/*
 * LIBRARY INITIALIZATION
 */

static const R_CallMethodDef CallEntries[] = {
  // Graph
  {"cg_graph", (DL_FUNC) &cg_graph, 0},
  {"cg_constant", (DL_FUNC) &cg_graph_add_constant, 3},
  {"cg_parameter", (DL_FUNC) &cg_graph_add_parameter, 3},
  {"cg_input", (DL_FUNC) &cg_graph_add_input, 2},
  {"cg_operator", (DL_FUNC) &cg_graph_add_operator, 4},
  {"cg_run", (DL_FUNC) &cg_graph_run, 3},
  {"cg_gradients", (DL_FUNC) &cg_graph_gradients, 5},
  // Function
  {"cg_function", (DL_FUNC) &cg_function, 2},
  // Internal
  {"bsum", (DL_FUNC) &bsum, 2},
  {"approx_gradients", (DL_FUNC) &approx_gradients, 6},
  // Math
  {"sigmoid", (DL_FUNC) &sigmoid, 1},
  {NULL, NULL, 0}
};

void R_init_cgraph(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
