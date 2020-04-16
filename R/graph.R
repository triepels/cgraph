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

#' Computational Graph
#'
#' Initialize a computational graph.
#'
#' @param eager logical scalar, should new nodes added to the graph be evaluated eagerly? Defaults to TRUE.
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
cg_graph <- function(eager = TRUE)
{
  .Call("cg_graph", eager, PACKAGE = "cgraph")
}

#' Retrieve Node
#'
#' Retrieve a node from a graph by name.
#'
#' @param graph cg_graph object, graph containing the node to be retrieved.
#' @param name character scalar, name of the node to be retrieved.
#'
#' @note In case multiple nodes share the same name, the last node added to the graph is retrieved.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add an input
#' a <- cg_input(name = "a")
#'
#' # Retrieve input a
#' b <- cg_graph_get(graph, "a")
#'
#' # Check equality
#' identical(a, b)
#'
#' @author Ron Triepels
#' @export
cg_graph_get <- function(graph, name)
{
  .Call("cg_graph_get", graph, name, PACKAGE = "cgraph")
}

#' Forward Pass
#'
#' Perform a forward pass to evaluate a given target node in a graph.
#'
#' @param graph cg_graph object, graph that is evaluated.
#' @param target cg_node object, node in the graph that is evaluated. Alternatively, argument \code{target} can be a character scalar denoting the name of the node in the graph that is evaluated.
#'
#' @note All nodes required to compute the target node must have a value or their value must be able to be computed at run-time. Only those nodes needed to compute the target node (including the target itself) are evaluated.
#'
#' The value of a node can be retrieved via the \code{values} data member of a \code{cg_node} object.
#'
#' If the name of the target node is supplied to argument \code{target}, a linear search is performed to retrieve the node from the graph. In case multiple nodes share the same name, the last node added to the graph is retrieved. Please note that this linear search can become relatively expensive for large graphs.
#'
#' @return None.
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
  if(is.character(target))
  {
    target <- cg_graph_get(graph, target)
  }

  invisible(.Call("cg_graph_forward", graph, target, PACKAGE = "cgraph"))
}

#' Backward Pass
#'
#' Perform a backward pass to evaluate the partial derivatives of a given target node with respect to the nodes in a graph.
#'
#' @param graph cg_graph object, graph that is differentiated.
#' @param target cg_node object, node in the graph that is differentiated. Alternatively, argument \code{target} can be a character scalar denoting the name of the node in the graph that is differentiated.
#' @param index numerical scalar, index of the target node that is differentiated. Defaults to NULL (i.e. all elements are differentiated element-wise).
#'
#' @note All nodes required to compute the target node must first have been evaluated by calling \link[cgraph:cg_graph_forward]{cg_graph_forward}. The target node is only differenated with respect to those nodes on which it directly or indirectly depends.
#'
#' In case the value of the target node is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated.
#'
#' The derivatives have the same shape as the values of the nodes. They can be retrieved via the \code{grad} data member of a \code{cg_node} object.
#'
#' If the name of the target node is supplied to argument \code{target}, a linear search is performed to retrieve the node from the graph. In case multiple nodes share the same name, the last node added to the graph is retrieved. Please note that this linear search can become relatively expensive for large graphs.
#'
#' @return None.
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
  if(is.character(target))
  {
    target <- cg_graph_get(graph, target)
  }

  invisible(.Call("cg_graph_backward", graph, target, index, PACKAGE = "cgraph"))
}

#' @author Ron Triepels
#' @export
print.cg_graph <- function(x, ...)
{
  invisible(.Call("cg_graph_print", x, PACKAGE = "cgraph"))
}
