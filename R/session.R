# Copyright 2018 Ron Triepels
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

# Session environment
session <- new.env()

# Active graph
session$graph <- NULL

#' Add Constant
#'
#' Add a constant node to the active graph.
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note Constants are ignored when differentiating a graph. The intended use of constants is that they are given a fixed value. However, it is still possible to change the value of constants when evaluating or differentiating a graph (see \link[cgraph]{run} and \link[cgraph]{gradients} for more details).
#'
#' The name of the constant node cannot be 'grad' as this is a reserved word.
#'
#' @return cg.node, constant.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add a constant with value 1 and name 'c' to the graph.
#' const(1, name = "c")
#'
#' @author Ron Triepels
const <- function(value = NULL, name = NULL)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$const(value, name)
}

#' Add Input
#'
#' Add an input node to the active graph.
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note The intended use of inputs is that they are not given a fixed value but behave as placeholders. Values can be supplied for inputs when evaluating or differentiating a graph (see \link[cgraph]{run} and \link[cgraph]{gradients} for more details).
#'
#' The name of the input node cannot be 'grad' as this is a reserved word.
#'
#' @return cg.node, input.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add an input with name 'x' to the graph.
#' input(name = "x")
#'
#' @author Ron Triepels
input <- function(value = NULL, name = NULL)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$input(value, name)
}

#' Add Parameter
#'
#' Add a parameter node to the active graph.
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar, name of the node (optional). In case \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note Parameters are assumed to be subject to some optimization process. Hence, their value might change over time.
#'
#' The name of the parameter node cannot be 'grad' as this is a reserved word.
#'
#' @return cg.node, parameter.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add a parameter with value 1 and name 'p' to the graph.
#' parm(1, name = "p")
#'
#' @author Ron Triepels
parm <- function(value = NULL, name = NULL)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$parm(value, name)
}

#' Add Operation
#'
#' Add an operation node to the graph.
#'
#' @param call call or symbol, operation performed by the node. Must evaluate to a numeric vector or array.
#' @param grads named list of calls, gradients of the input nodes that are consumed by the operation in argument \code{call}. Is ignored when the elements are not named.
#' @param binding named list or environment, binds the variables in the calls of argument \code{call} and \code{grads} to the symbols of the nodes in the graph.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note The operation to be performed by the node should be provided as a call to argument \code{call}. If this operation consumes any other nodes in the graph, then the gradients of the operation with respect to these input nodes should be supplied as a call to argument \code{gradients}. These gradients must be a function of each input's gradient. The special reserved word 'grad' evaluates to this gradient at run-time and can be used in the call of each input's gradient as placeholder.
#'
#' Any variabes in the calls of the node (both supplied to argument \code{call} and \code{gradients}) should be bind to the symbols of the nodes in the graph. This can be done by supplying the names of the variables and the corresponding nodes to which the variables should bind to \code{binding}. At run-time, the symbols of the nodes are substituted for the variables in the calls.
#'
#' The name of the operation node cannot be 'grad' as this is a reserved word.
#'
#' @return cg.node, operation.
#'
#' @author Ron Triepels
opr <- function(call, grads, args, name = NULL)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$opr(call, grads, args, name)
}

val <- function(name)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$val(name)
}

set <- function(name, value)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$set(name, value)
}

#' Evaluate the Graph
#'
#' Evaluate node \code{name} in the active graph.
#'
#' @param name character scalar or symbol, name of the node that is evaluated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#'
#' @note All nodes required to compute node \code{name} must have a value or their value must be able to be computed at run-time. Nodes can be assigned a value when they are created. Alternatively, argument \code{values} can be used to substitute values for nodes that do not have a value (e.g. inputs) or to fix their values.
#'
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. Values of nodes that have not changed or are not evaluated are not returned.
#'
#' @return environment, the value of node \code{name} including the values of all ancestors of \code{name}.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add an input.
#' a <- input(name = "a")
#'
#' # Square the input (i.e. b = a^2).
#' b <- cg.pow(a, const(2), name = "b")
#'
#' # Evaluate b at a = 2.
#' values <- run(b, list(a = 2))
#'
#' # Retrieve the value of b.
#' values$b
#'
#' @author Ron Triepels
run <- function(name, values = list())
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$run(name, values)
}

#' Calculate Gradients
#'
#' Differentiate the active graph with respect to node \code{name} by reverse automatic differentiation.
#'
#' @param name character scalar or symbol, name of the node that is differentiated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#'
#' @note All nodes required to compute node \code{name} must have a value, or their value must be able to be computed at run-time. The values of nodes can be obtained by first evaluating node \code{name} in the graph using function \link[cgraph]{run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' Currently, the cgraph package can only differentiate scalar target nodes. In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is to be differentiated.
#'
#' The gradients of all ancestor nodes of node \code{name} are returned. Constant nodes are not differentiated and their gradients are not returned. The gradients have the same shape as the nodes.
#'
#' @return environment, the gradients of all nodes with respect to target node \code{name}.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' a <- parm(2, name = "a")
#' b <- parm(4, name = "b")
#'
#' # Perform some operations on the parameters.
#' c <- cg.sin(a) + cg.cos(b) - cg.tan(a)
#'
#' # Differentiate the graph with respect to c.
#' grads <- gradients(c, run(c))
#'
#' # Retrieve the gradient of c with respect to a.
#' grads$a
#'
#' @author Ron Triepels
gradients <- function(name, values = new.env(), index = 1)
{
  if(is.null(session$graph))
  {
    stop("No active graph set", call. = FALSE)
  }

  session$graph$gradients(name, values, index)
}
