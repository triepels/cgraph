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

#' @useDynLib cgraph
NULL

# Session environment that stores the active graph
session <- new.env()

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
#' @export
cg_graph <- function()
{
  graph <- .Call("cg_graph", PACKAGE = "cgraph")

  session$graph <- graph

  graph
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
#' x <- cg_graph()
#'
#' # Initialize another computational graph. It becomes the active graph.
#' y <- cg_graph()
#'
#' # Set x to be the active graph
#' cg_active(y)
#'
#' @author Ron Triepels
#' @export
cg_active <- function(graph)
{
  if(!is.environment(graph) || !("cg_graph" %in% class(graph)))
  {
    stop("argument 'graph' must be a cg_graph object", call. = FALSE)
  }

  session$graph <- graph
}

#' Add Constant
#'
#' Add a constant node to the active graph.
#'
#' @param value numeric vector or array, value of the node.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Constant nodes are ignored when differentiating a graph.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Add a constant with value 1 and name 'a' to the graph.
#' a <- cg_constant(1, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_constant <- function(value, name = NULL)
{
  graph <- session$graph

  if(is.null(graph))
  {
    stop("no active graph set", call. = FALSE)
  }

  .Call("cg_constant", graph, value, name, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter node to the active graph.
#'
#' @param value numeric vector or array, value of the node.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Parameters are assumed to be subject to some optimization process. Hence, their value might change over time.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Add a parameter with value 1 and name 'a' to the graph.
#' a <- cg_parameter(1, name = "a")
#'
#' @author Ron Triepels
#' @export
cg_parameter <- function(value, name = NULL)
{
  graph <- session$graph

  if(is.null(graph))
  {
    stop("no active graph set", call. = FALSE)
  }

  .Call("cg_parameter", graph, value, name, PACKAGE = "cgraph")
}

#' Add Input
#'
#' Add an input node to the active graph.
#'
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Inputs cannot be assigned a fixed value but behave as placeholders. Values can be assigned to inputs when evaluating or differentiating a graph (see \link[cgraph]{cg_run} and \link[cgraph]{cg_gradients} for more details).
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Add a parameter with name 'a' to the graph.
#' a <- cg_input(name = "a")
#'
#' @author Ron Triepels
#' @export
cg_input <- function(name = NULL)
{
  graph <- session$graph

  if(is.null(graph))
  {
    stop("no active graph set", call. = FALSE)
  }

  .Call("cg_input", graph, name, PACKAGE = "cgraph")
}

#' Add Operation
#'
#' Add an operation node to the active graph.
#'
#' @param fun cg_function object, operation performed by the node.
#' @param inputs list, the nodes that are consumed by the operation.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Any objects that are supplied to argument \code{inputs} that are not cg_node objects are automatically converted to constant nodes.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Create a custom negation function
#' f <- cg_function(def = function(x) -x, grads = list(function(x, val, grad) -grad))
#'
#' # Add a negation operator with name 'a' to the graph.
#' a <- cg_operator(f, list(10), name = "a")
#'
#' @author Ron Triepels
#' @export
cg_operator <- function(fun, inputs, name = NULL)
{
  graph <- session$graph

  if(is.null(graph))
  {
    stop("no active graph set", call. = FALSE)
  }

  .Call("cg_operator", graph, fun, inputs, name, PACKAGE = "cgraph")
}

#' Evaluate the Graph
#'
#' Evaluate a node in a graph.
#'
#' @param graph cg_graph object, graph that is evaluated.
#' @param target cg_node object, node in the graph that is evaluated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
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
#' values <- cg_run(b, list(a = 2))
#'
#' # Retrieve the value of b
#' values$b
#'
#' @author Ron Triepels
#' @export
cg_run <- function(graph, target, values = new.env(parent = emptyenv()))
{
  if(is.list(values))
  {
    values <- list2env(values, parent = emptyenv())
  }

  .Call("cg_run", graph, target, values, PACKAGE = "cgraph")
}

#' Calculate Gradients
#'
#' Differentiate the graph with respect to a node by reverse automatic differentiation.
#'
#' @param graph cg_graph object, graph that is differentiated.
#' @param target cg_node object, node in the graph that is differentiated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#'
#' @note All nodes required to compute the node must have a value, or their value must be able to be computed at run-time. The values of nodes can be obtained by first evaluating function \link[cgraph]{cg_run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
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
#' # Differentiate the graph with respect to c.
#' grads <- cg_gradients(c, cg_run(c))
#'
#' # Retrieve the gradient of c with respect to a
#' grads$a
#'
#' @author Ron Triepels
#' @export
cg_gradients <- function(graph, target, values = new.env(parent = emptyenv()), index = 1)
{
  if(is.list(values))
  {
    values <- list2env(values, parent = emptyenv())
  }

  .Call("cg_gradients", graph, target, values, new.env(parent = emptyenv()), index, PACKAGE = "cgraph")
}

#' @export
print.cg_graph <- function(x, ...)
{
  cat("<cg_graph>\n")
}

#' @export
print.cg_constant <- function(x, ...)
{
  cat(sprintf("<cg_constant: %s>\n", x$name))
}

#' @export
print.cg_parameter <- function(x, ...)
{
  cat(sprintf("<cg_parameter: %s>\n", x$name))
}

#' @export
print.cg_input <- function(x, ...)
{
  cat(sprintf("<cg_input: %s>\n", x$name))
}

#' @export
print.cg_operator <- function(x, ...)
{
  cat(sprintf("<cg_operator: %s>\n", x$name))
}
