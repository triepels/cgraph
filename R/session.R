# Copyright 2020 Ron Triepels
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Get Active Graph
#'
#' Get the graph that is currently active.
#'
#' @return cg_graph object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Retrieve the graph from the session
#' cg_session_graph()
#'
#' @author Ron Triepels
#' @export
cg_session_graph <- function()
{
  .Call("cg_session_graph", PACKAGE = "cgraph")
}

#' Change Active Graph
#'
#' Set a graph to be the active graph.
#'
#' @param graph cg_graph object, the graph that is activated.
#'
#' @note Any nodes that are created are automatically added to the active graph. This also applies to operations that are created by overloaded S3 functions that do not follow the cg_<name> naming convention (such as primitive inflix functions '+' and '-').
#'
#' Only one graph can be active at a time. The active graph can be changed by calling this function on another cg_graph object.
#'
#' @return none.
#'
#' @examples # Initialize a computational graph
#' graph1 <- cg_graph()
#'
#' # Initialize another computational graph. It becomes the active graph.
#' graph2 <- cg_graph()
#'
#' # Set graph1 to be the active graph
#' cg_session_set_graph(graph1)
#'
#' @author Ron Triepels
#' @export
cg_session_set_graph <- function(graph)
{
  invisible(.Call("cg_session_set_graph", graph, PACKAGE = "cgraph"))
}

#' @export
print.cg_session <- function(x, ...)
{
  cat("<cg_session>")
}
