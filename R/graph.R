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
#' graph <- cg_graph()
#'
#' @author Ron Triepels
#' @useDynLib cgraph
#' @export
cg_graph <- function()
{
  .Call("cg_graph", PACKAGE = "cgraph")
}

#' Forward Pass
#'
#' Perform a forward pass to evaluate a given target node in a graph.
#'
#' @param graph cg_graph object, graph that is evaluated.
#' @param target cg_node object, node in the graph that is evaluated.
#'
#' @note All nodes required to compute the target node must have a value or their value must be able to be computed at run-time. Only those nodes needed to compute the target node (including the target itself) are evaluated.
#'
#' The value of a node can be retrieved via the \code{values} data member of a \code{cg_node} object.
#'
#' @return none.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add an input
#' a <- cg_input(name = "a")
#'
#' # Square the input (i.e. b = a^2)
#' b <- cg_pow(a, 2, name = "b")
#'
#' # Set a equal to 2
#' a$value <- 2
#'
#' # Perform forward pass
#' cg_graph_forward(graph, b)
#'
#' # Retrieve the value of b
#' b$value
#'
#' @author Ron Triepels
#' @export
cg_graph_forward <- function(graph, target)
{
  invisible(.Call("cg_graph_forward", graph, target, PACKAGE = "cgraph"))
}

#' Backward Pass
#'
#' Perform a backward pass to evaluate the partial derivatives of a given target node with respect to the nodes in a graph.
#'
#' @param graph cg_graph object, graph that is differentiated.
#' @param target cg_node object, node in the graph that is differentiated.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to NULL (i.e. all elements are differentiated element-wise).
#'
#' @note All nodes required to compute the target node must first have been evaluated by calling \link[cgraph:cg_graph_forward]{cg_graph_forward}. The target node is only differenated with respect to those nodes on which it directly or indirectly depends.
#'
#' In case the value of the target node is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated.
#'
#' The derivatives have the same shape as the values of the nodes. They can be retrieved via the \code{grad} data member of a \code{cg_node} object.
#'
#' @return none.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add an input
#' a <- cg_input(name = "a")
#'
#' # Add a parameter
#' b <- cg_parameter(4, name = "b")
#'
#' # Perform some operations
#' c <- cg_sin(a) + cg_cos(b) - cg_tan(a)
#'
#' # Set a equal to 2
#' a$value <- 2
#'
#' # Perform forward pass
#' cg_graph_forward(graph, c)
#'
#' # Perform backward pass
#' cg_graph_backward(graph, c)
#'
#' # Retrieve the derivative of c with respect to b
#' b$grad
#'
#' @author Ron Triepels
#' @export
cg_graph_backward <- function(graph, target, index = NULL)
{
  invisible(.Call("cg_graph_backward", graph, target, index, PACKAGE = "cgraph"))
}

#' Evaluate the Graph
#'
#' Evaluate a given target node in a graph.
#'
#' @param graph cg_graph object, graph that is evaluated.
#' @param target cg_node object, node in the graph that is evaluated.
#' @param values named list or environment, values that are subsituted for the input nodes in the graph.
#'
#' @note This function is deprecated and will be removed in the next major release. Please use function \link[cgraph:cg_graph_forward]{cg_graph_forward} to evaluate a graph instead.
#'
#' All nodes required to compute the target node must have a value or their value must be able to be computed at run-time. Argument \code{values} can be used to substitute values for input nodes that do not have a value. Only those nodes needed to compute the node are evaluated and their values are returned.
#'
#' @return environment, the value of the node including the value of all ancestors of the node in the graph.
#'
#' @author Ron Triepels
#' @export
cg_graph_run <- function(graph, target, values = new.env())
{
  .Deprecated("cg_graph_forward")

  if(is.list(values))
  {
    values <- list2env(values)
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
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to NULL (i.e. all elements are differentiated element-wise).
#'
#' @note This function is deprecated and will be removed in the next major release. Please use function \link[cgraph:cg_graph_backward]{cg_graph_backward} to differentiate a graph instead.
#'
#' All nodes required to compute the target node must have a value, or their value must be able to be computed at run-time. The values of the nodes can be obtained by first evaluating function \link[cgraph]{cg_graph_run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated.
#'
#' The gradients of all ancestors of the node are returned. The gradients have the same shape as the values of the nodes.
#'
#' @return environment, the gradients of all ancestors of the node (including the target node itself) with respect to the target node.
#'
#' @author Ron Triepels
#' @export
cg_graph_gradients <- function(graph, target, values = new.env(), index = NULL)
{
  .Deprecated("cg_graph_backward")

  if(is.list(values))
  {
    values <- list2env(values)
  }

  .Call("cg_graph_gradients", graph, target, values, new.env(), index, PACKAGE = "cgraph")
}

#' @export
print.cg_graph <- function(x, ...)
{
  cat("<cg_graph>\n")
}
