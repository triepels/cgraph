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

#' Computational Graph
#'
#' The \code{cgraph} class facilitates the construction, evaluation, and differentiation of computaiontal graphs in R.
#'
#' @section Usage:
#' \preformatted{x <- cgraph$new()}
#'
#' @section Members:
#' \describe{
#' \item{$nodes}{named list, symbols of the nodes.}
#' \item{$values}{environment, values of the nodes.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{$initialize}{initialize a computational graph, see \link[cgraph]{cg.initialize}.}
#' \item{$name}{generate a default name for a node, see \link[cgraph]{cg.name}.}
#' \item{$const}{add a constant node to the graph, see \link[cgraph]{cg.const}.}
#' \item{$input}{add an input node to the graph, see \link[cgraph]{cg.input}.}
#' \item{$parm}{add a parameter node to the graph, see \link[cgraph]{cg.parm}.}
#' \item{$get.parms}{list all parameters and their values, see \link[cgraph]{cg.get.parms}.}
#' \item{$add.parms}{add parameters to the graph, see \link[cgraph]{cg.add.parms}.}
#' \item{$opr}{add an operation node to the graph, see \link[cgraph]{cg.opr}.}
#' \item{$active}{set the graph to be the active graph, see \link[cgraph]{cg.active}.}
#' \item{$run}{evaluate a node in the graph, see \link[cgraph]{cg.run}.}
#' \item{$gradients}{differentiate the graph by reverse automatic differentiation, see \link[cgraph]{cg.gradients}.}
#' \item{$approx.grad}{differentiate the graph by numerical differentiation, see \link[cgraph]{cg.approx.grad}.}
#' \item{$adj.mat}{retrieve the adjacency matrix of the graph, see \link[cgraph]{cg.adj.mat}.}
#' \item{$plot}{plot the topology of the graph, see \link[cgraph]{cg.plot}.}
#' }
#'
#' @note Some of the methods listed above have a wrapper function that calls the method on the current active graph. For example, a parameter can be added to the current active graph by calling \link[cgraph]{parm} instead of calling \link[cgraph]{cg.parm} on the cgraph object.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' a <- parm(2, name = "a")
#' b <- parm(4, name = "b")
#'
#' # Perform some operations on the parameters.
#' c <- sin(a) + cos(b) - tan(a)
#'
#' # Evaluate c.
#' values <- run(c)
#'
#' # Retrieve the value of c (the node is called 'node7' in the graph).
#' values$node7
#'
#' # Differentiate the graph with respect to c.
#' grads <- gradients(c, values)
#'
#' # Retrieve the gradient of c with respect to a.
#' grads$a
#'
#' @name cgraph
#' @author Ron Triepels
NULL

cgraph <- R6Class(
  classname = "cgraph",
  class = T,
  cloneable = T,
  public = list(
    nodes = NULL,
    values = NULL
  )
)

#' Computational Graph
#'
#' Initialize a computational graph.
#'
#' @details \code{$new()}
#'
#' @note The cgraph object is set to be the active graph. Any nodes that are created by wrapper function \link[cgraph]{const}, \link[cgraph]{input}, \link[cgraph]{parm}, or \link[cgraph]{opr} will be added to this graph. Also, when printing a node, its value will be evaluated in the active graph. You can change the active graph by calling the \link[cgraph]{cg.active} method on another cgraph object.
#'
#' @return cgraph object.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' @name cg.initialize
#' @author Ron Triepels
cgraph$public_methods$initialize <- function()
{
  values <- new.env(parent = asNamespace("cgraph"))

  .Call("cgraph", self, values, PACKAGE = "cgraph")

  self$active()
}

#' Generate Name
#'
#' Generate a default name for a node.
#'
#' @details \code{$name()}
#'
#' @note The auto-generated name is not guaranteed to be unique.
#'
#' There is a wrapper function \link[cgraph]{name} that calls this method on the current active graph.
#'
#' @return character scalar, auto-generated name for the node.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Generate a name.
#' x$name()
#'
#' @name cg.name
#' @author Ron Triepels
cgraph$public_methods$name <- function()
{
  .Call("cg_gen_name", self, PACKAGE = "cgraph")
}

#' Add Constant
#'
#' Add a constant node to the graph.
#'
#' @details \code{$const(value, name)}
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar or symbol, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note Constants are ignored when differentiating a graph. The intended use of constants is that they are given a fixed value. However, it is still possible to change the value of constants when evaluating or differentiating a graph (see \link[cgraph]{run} and \link[cgraph]{gradients} for more details).
#'
#' The name of the constant node cannot be 'grad' as this is a reserved word.
#'
#' There is a wrapper function \link[cgraph]{const} that calls this method on the current active graph.
#'
#' @return cg.node, constant.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add a constant with value 1 and name 'c' to the graph.
#' x$const(1, name = "c")
#'
#' @name cg.const
#' @author Ron Triepels
cgraph$public_methods$const <- function(value, name)
{
  if(missing(value))
  {
    value <- NULL
  }

  if(missing(name))
  {
    name <- NULL
  }

  .Call("cg_add_constant", value, name, self, PACKAGE = "cgraph")
}

#' Add Input
#'
#' Add an input node to the graph.
#'
#' @details \code{$input(value, name)}
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar or symbol, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note The intended use of inputs is that they are not given a fixed value but behave as placeholders. Values can be supplied for inputs when evaluating or differentiating a graph (see \link[cgraph]{run} and \link[cgraph]{gradients} for more details).
#'
#' The name of the input node cannot be 'grad' as this is a reserved word.
#'
#' There is a wrapper function \link[cgraph]{input} that calls this method on the current active graph.
#'
#' @return cg.node, input.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add an input with name 'x' to the graph.
#' x$input(name = "x")
#'
#' @name cg.input
#' @author Ron Triepels
cgraph$public_methods$input <- function(value, name)
{
  if(missing(value))
  {
    value <- NULL
  }

  if(missing(name))
  {
    name <- NULL
  }

  .Call("cg_add_input", value, name, self, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter node to the graph.
#'
#' @details \code{$parm(value, name)}
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note Parameters are assumed to be subject to some optimization process. Hence, their value might change over time.
#'
#' The name of the parameter node cannot be 'grad' as this is a reserved word.
#'
#' There is a wrapper function \link[cgraph]{parm} that calls this method on the current active graph.
#'
#' @return cg.node, parameter.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add a parameter with value 1 and name 'p' to the graph.
#' x$parm(1, name = "p")
#'
#' @name cg.parm
#' @author Ron Triepels
cgraph$public_methods$parm <- function(value, name)
{
  if(missing(value))
  {
    value <- NULL
  }

  if(missing(name))
  {
    name <- NULL
  }

  .Call("cg_add_parameter", value, name, self, PACKAGE = "cgraph")
}

#' Get Parameters
#'
#' List all parameters and their values.
#'
#' @details \code{$get.parms()}
#'
#' @return named list, parameters of the graph.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' x$add.parms(prm1 = 1, prm2 = 2, prm3 = 3)
#'
#' # List the parameters.
#' x$get.parms()
#'
#' @name cg.get.parms
#' @author Ron Triepels
cgraph$public_methods$get.parms <- function()
{
  .Call("cg_get_parms", self, PACKAGE = "cgraph")
}

#' Add Parameters
#'
#' Add parameters to the graph.
#'
#' @details \code{$add.parms(..., parms = NULL)}
#'
#' @param ... numeric vectors or arrays, the values of the parameters. Is ignored when \code{parms} is not \code{NULL}.
#' @param parms named list, the parameters that are to be added to the graph.
#'
#' @note Parameters can be named by providing named arguments to \code{...} or by naming the elements of argument \code{parms}. In case no names are provided, parameters are tried to be added to the graph under an auto-generated name. No default value is set for parameters with value \code{NULL}.
#'
#' @return nothing.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' x$add.parms(prm1 = 1, prm2 = 2, prm3 = 3)
#'
#' # List the parameters.
#' x$get.parms()
#'
#' @name cg.add.parms
#' @author Ron Triepels
cgraph$public_methods$add.parms <- function(..., parms = NULL)
{
  if(is.null(parms))
  {
    parms <- list(...)
  }

  invisible(.Call("cg_add_parms", parms, self, PACKAGE = "cgraph"))
}

#' Add Operation
#'
#' Add an operation node to the graph.
#'
#' @details \code{$opr(call, grads, binding, name)}
#'
#' @param call call or symbol, operation performed by the node. Must evaluate to a numeric vector or array.
#' @param grads named list of calls, gradients of the input nodes that are consumed by the operation in argument \code{call}. Is ignored when the elements are not named.
#' @param binding named list or environment, binds the variables in the calls of argument \code{call} and \code{grads} to the symbols of the nodes in the graph.
#' @param name character scalar or symbol, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note The operation to be performed by the node should be provided as a call to argument \code{call}. If this operation consumes any other nodes in the graph, then the gradients of the operation with respect to these input nodes should be supplied as a call to argument \code{gradients}. These gradients must be a function of each input's gradient. The special reserved word 'grad' evaluates to this gradient at run-time and can be used in the call of each input's gradient as placeholder.
#'
#' Any variabes in the calls of the node (both supplied to argument \code{call} and \code{gradients}) should be bind to the symbols of the nodes in the graph. This can be done by supplying the names of the variables and the corresponding nodes to which the variables should bind to \code{binding}. At run-time, the symbols of the nodes are substituted for the variables in the calls.
#'
#' The name of the operation node cannot be 'grad' as this is a reserved word.
#'
#' There is a wrapper function \link[cgraph]{opr} that calls this method on the current active graph.
#'
#' @return cg.node, operation.
#'
#' @name cg.opr
#' @author Ron Triepels
cgraph$public_methods$opr <- function(call, grads, binding, name)
{
  if(is.list(binding))
  {
    binding <- list2env(binding)
  }

  if(missing(name))
  {
    name <- NULL
  }

  .Call("cg_add_operation", call, grads, binding, name, self, PACKAGE = "cgraph")
}

#' Change Active Graph
#'
#' Set the graph to be the active graph.
#'
#' @details \code{$active()}
#'
#' @note Any nodes that are created are automatically added to the active graph. This also applies to operations that are created by overloaded S3 functions that do not follow the cg.<name> naming convention (such as primitive functions '+' and '-').
#'
#' Only one graph can be active at a time. The active graph can be changed by calling \link[cgraph]{cg.active} on another cgraph object.
#'
#' @return none.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Initialize another computational graph. It becomes the current active graph.
#' y <- cgraph$new()
#'
#' # Set graph x to be the active graph.
#' x$active()
#'
#' @name cg.active
#' @author Ron Triepels
cgraph$public_methods$active <- function()
{
  assign("graph", self, envir = session)
}

#' Evaluate the Graph
#'
#' Evaluate node \code{name} in the graph.
#'
#' @details \code{$run(name, values = list())}
#'
#' @param name character scalar, name of the node that is evaluated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#'
#' @note All nodes required to compute node \code{name} must have a value or their value must be able to be computed at run-time. Nodes can be assigned a value when they are created. Alternatively, argument \code{values} can be used to substitute values for nodes that do not have a value (e.g. inputs) or to fix their values.
#'
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. Values of nodes that have not changed or are not evaluated are not returned.
#'
#' There is a wrapper function \link[cgraph]{run} that calls this method on the current active graph.
#'
#' @return environment, the value of node \code{name} including the values of all ancestors of \code{name}.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add an input.
#' a <- x$input(name = "a")
#'
#' # Square the input (i.e. b = a^2).
#' b <- cg.pow(a, x$const(2), name = "b")
#'
#' # Evaluate the graph at a = 2.
#' values <- x$run(b, list(a = 2))
#'
#' # Retrieve the value of b.
#' values$b
#'
#' @name cg.run
#' @author Ron Triepels
cgraph$public_methods$run <- function(name, values = list())
{
  if(is.list(values))
  {
    values <- list2env(values)
  }

  .Call("cg_run", name, values, self, PACKAGE = "cgraph")
}

#' Calculate Gradients
#'
#' Differentiate the graph with respect to node \code{name} by reverse automatic differentiation.
#'
#' @details \code{$gradients(name, values, index = 1)}
#'
#' @param name character scalar, name of the node that is differentiated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#'
#' @note All nodes required to compute node \code{name} must have a value, or their value must be able to be computed at run-time. The values of nodes can be obtained by first evaluating node \code{name} in the graph using function \link[cgraph]{cg.run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' Currently, the cgraph package can only differentiate scalar target nodes. In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is to be differentiated.
#'
#' The gradients of all ancestor nodes of node \code{name} are returned. Constant nodes are not differentiated and their gradients are not returned. The gradients have the same shape as the nodes.
#'
#' There is a wrapper function \link[cgraph]{gradients} that calls this method on the current active graph.
#'
#' @return environment, the gradients of all nodes with respect to target node \code{name}.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' a <- x$parm(2, name = "a")
#' b <- x$parm(4, name = "b")
#'
#' # Perform some operations on the parameters.
#' c <- sin(a) + cos(b) - tan(a)
#'
#' # Differentiate the graph with respect to c.
#' grads <- x$gradients(c, x$run(c))
#'
#' # Retrieve the gradient of c with respect to a.
#' grads$a
#'
#' @name cg.gradients
#' @author Ron Triepels
cgraph$public_methods$gradients <- function(name, values = list(), index = 1)
{
  if(is.list(values))
  {
    values <- list2env(values)
  }

  .Call("cg_gradients", name, values, index, self, PACKAGE = "cgraph")
}

#' Approximate Gradients
#'
#' Differentiate node \code{x} with respect to node \code{y} by numerical differentiation.
#'
#' @details \code{$approx.grad(x, y, values = list(), index = 1, eps = 1e-4)}
#'
#' @param x character scalar, name of the node.
#' @param y character scalar, name of the node.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#' @param eps numeric scalar, step size. Defaults to 1e-4.
#'
#' @note All nodes required to compute node \code{name} must have a value, or their value must be able to be computed at run-time. The values of nodes can be obtained by first evaluating node \code{name} in the graph using function \code{$run()}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' The graph is differentiation by the symmetric difference quotient. This method can only be used to differentiate scalars. In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated. The caluclated gradient has the same shape as the value of node \code{y}.
#'
#' Numerical differentiation is subject to estimation error and can be very slow. Therefore, this function should only be used for testing purposes.
#'
#' There is a wrapper function \link[cgraph]{approx.grad} that calls this method on the current active graph.
#'
#' @return numeric vector or array, the derivative of \code{x} with respect to \code{y}.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' a <- x$parm(2, name = "a")
#' b <- x$parm(4, name = "b")
#'
#' # Perform some operations on the parameters.
#' c <- sin(a) + cos(b) - tan(a)
#'
#' # Differentiate the graph with respect to c.
#' grads <- x$gradients(c, x$run(c))
#'
#' # Retrieve the gradient of c with respect to a.
#' grads$a
#'
#' # Approximate the same gradient with numerical differentiation.
#' x$approx.grad(c, a)
#'
#' @name cg.approx.grad
#' @author Ron Triepels
cgraph$public_methods$approx.grad <- function(x, y, values = list(), index = 1, eps = 1e-4)
{
  if(is.list(values))
  {
    values <- list2env(values)
  }

  .Call("cg_approx_grad", x, y, values, index, eps, self, PACKAGE = "cgraph")
}

#' Adjacency Matrix
#'
#' Retrieve the adjacency matrix of the graph.
#'
#' @details \code{$adj.mat()}
#'
#' @return numeric matrix, the adjacency matrix of the graph.
#'
#' @name cg.adj.mat
#' @author Ron Triepels
cgraph$public_methods$adj.mat <- function()
{
  .Call("cg_adj_mat", self, PACKAGE = "cgraph")
}

#' Plot
#'
#' Plot the topology of the graph.
#'
#' @details \code{$plot(...)}
#'
#' @param ... additional arguments that can be passed on to the plot function of the Rgraphiz package.
#'
#' @note A visual representation of the graph might be usefull for debugging purposes. This functions requires the Rgraphviz package.
#'
#' @return none.
#'
#' @name cg.plot
#' @author Ron Triepels
cgraph$public_methods$plot <- function(...)
{
  Rgraphviz::plot(new("graphAM", adjMat = self$adj.mat(), edgemode = "directed"), ...)
}

#' Plot
#'
#' Plot the topology of a graph.
#'
#' @param x cgraph object, computational graph that is to be plotted.
#' @param ... additional arguments that can be passed on to the plot function of the Rgraphiz package.
#'
#' @note A visual representation of the graph might be usefull for debugging purposes. This functions requires the Rgraphviz package.
#'
#' @return none.
#'
#' @author Ron Triepels
plot.cgraph <- function(x, ...)
{
  x$plot(...)
}
