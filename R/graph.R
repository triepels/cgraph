# Copyright 2019 Ron Triepels
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

#' Computational Graph
#'
#' Initialize a computational graph
#'
#' @note The graph is automatically set to be the active graph.
#'
#' @return cg_graph object.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' @author Ron Triepels
#' @useDynLib cgraph
#' @export
cg_graph <- function()
{
  .Call("cg_graph", PACKAGE = "cgraph")
}

#' Evaluate the Graph
#'
#' Evaluate a given target node in a graph.
#'
#' @param graph cg_graph object, graph that is evaluated.
#' @param target cg_node object, node in the graph that is evaluated.
#' @param values named list or environment, values that are subsituted for the input nodes in the graph.
#'
#' @note All nodes required to compute the node must have a value or their value must be able to be computed at run-time. Argument \code{values} can be used to substitute values for input nodes that do not have a value. Only those nodes needed to compute the node are evaluated and their values are returned.
#'
#' @return environment, the value of the node including the value of all ancestors of the node in the graph.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Add an input
#' a <- cg_input(name = "a")
#'
#' # Square the input (i.e. b = a^2)
#' b <- cg_pow(a, 2, name = "b")
#'
#' # Evaluate b at a = 2
#' values <- cg_graph_run(x, b, list(a = 2))
#'
#' # Retrieve the value of b
#' values$b
#'
#' @author Ron Triepels
#' @export
cg_graph_run <- function(graph, target, values = new.env())
{
  if(is.list(values))
  {
    values <- list2env(values, parent = emptyenv())
  }

  .Call("cg_graph_run", graph, target, values, PACKAGE = "cgraph")
}

#' Calculate Gradients
#'
#' Differentiate a graph with respect to a given target node by reverse automatic differentiation.
#'
#' @param graph cg_graph object, graph that is differentiated.
#' @param target cg_node object, node in the graph that is differentiated.
#' @param values named list or environment, values that are subsituted for the input nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#'
#' @note All nodes required to compute the node must have a value, or their value must be able to be computed at run-time. The values of nodes can be obtained by first evaluating function \link[cgraph]{cg_graph_run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' Currently, the cgraph package can only differentiate scalar target nodes. In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated.
#'
#' The gradients of all ancestors of the node are returned. The gradients have the same shape as the values of the nodes.
#'
#' @return environment, the gradients of all ancestors of the node (including the target node itself) with respect to the target node.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Add some parameters
#' a <- cg_parameter(2, name = "a")
#' b <- cg_parameter(4, name = "b")
#'
#' # Perform some operations on the parameters
#' c <- cg_sin(a) + cg_cos(b) - cg_tan(a)
#'
#' # Differentiate the graph with respect to c
#' grads <- cg_graph_gradients(x, c, cg_graph_run(x, c))
#'
#' # Retrieve the gradient of c with respect to a
#' grads$a
#'
#' @author Ron Triepels
#' @export
cg_graph_gradients <- function(graph, target, values = new.env(), index = 1)
{
  if(is.list(values))
  {
    values <- list2env(values, parent = emptyenv())
  }

  .Call("cg_graph_gradients", graph, target, values, new.env(), index, PACKAGE = "cgraph")
}

#' @export
print.cg_graph <- function(x, ...)
{
  cat("<cg_graph>\n")
}
