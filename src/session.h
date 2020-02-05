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

#ifndef SESSION_H
#define SESSION_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * SESSION STRUCT
 */

typedef struct
{
    SEXP graph;
} cg_session_t;

/*
 * SESSION DECLARATION
 */

extern cg_session_t session;

/*
 * PUBLIC METHODS
 */

SEXP cg_session_graph();

SEXP cg_session_set_graph(SEXP graph);

#endif
