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
#' \item{$initialize}{initialize a computational graph, see \link[cgraph]{cg_initialize}.}
#' \item{$const}{add a constant node to the graph, see \link[cgraph]{cg_const}.}
#' \item{$input}{add an input node to the graph, see \link[cgraph]{cg_input}.}
#' \item{$parm}{add a parameter node to the graph, see \link[cgraph]{cg_parm}.}
#' \item{$get.parms}{list all parameters and their values, see \link[cgraph]{cg_get_parms}.}
#' \item{$add.parms}{add parameters to the graph, see \link[cgraph]{cg_add_parms}.}
#' \item{$opr}{add an operation node to the graph, see \link[cgraph]{cg_opr}.}
#' \item{$active}{set the graph to be the active graph, see \link[cgraph]{cg_active}.}
#' \item{$val}{get the value of a node in the graph, see \link[cgraph]{cg_val}.}
#' \item{$set}{set the value of a node in the graph, see \link[cgraph]{cg_set}.}
#' \item{$run}{evaluate a node in the graph, see \link[cgraph]{cg_run}.}
#' \item{$gradients}{differentiate the graph by reverse automatic differentiation, see \link[cgraph]{cg_gradients}.}
#' \item{$adj.mat}{retrieve the adjacency matrix of the graph, see \link[cgraph]{cg_adj_mat}.}
#' \item{$plot}{plot the topology of the graph, see \link[cgraph]{cg_plot}.}
#' }
#'
#' @note Some of the methods listed above have a wrapper function that calls the method on the current active graph. For example, a parameter can be added to the current active graph by calling \link[cgraph]{parm} instead of calling \link[cgraph]{cg_parm} on the cgraph object of the currently active graph.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add an input with name 'a' to the graph.
#' a <- input(name = "a")
#'
#' # Add a parameter with value 4 and name 'b' to the graph.
#' b <- parm(4, name = "b")
#'
#' # Perform some operations (i.e. c = exp(a * b)).
#' c <- cg_exp(a * b, name = "c")
#'
#' # Evaluate c at a = 2.
#' values <- run(c, list(a = 2))
#'
#' # Retrieve the value of c.
#' values$c
#'
#' # Differentiate the graph with respect to c.
#' grads <- gradients(c, values)
#'
#' # Retrieve the gradient of c with respect to b.
#' grads$b
#'
#' @name cgraph
#' @author Ron Triepels
#' @importFrom R6 R6Class
#' @useDynLib cgraph
NULL

#' @export
cgraph <- R6Class(
  classname = "cgraph",
  class = TRUE,
  cloneable = TRUE,
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
#' @note The cgraph object is set to be the active graph. Nodes that are created by wrapper function \link[cgraph]{const}, \link[cgraph]{input}, \link[cgraph]{parm}, or \link[cgraph]{opr} are added to the graph that is currently active. Also, wrapper function \link[cgraph]{get} and \link[cgraph]{set} operate on this graph. You can change the active graph by creating a new cgraph object using method \link[cgraph]{cg_initialize} or by calling the \link[cgraph]{cg_active} method on another cgraph object.
#'
#' @return cgraph object.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' @name cg_initialize
#' @author Ron Triepels
cgraph$public_methods$initialize <- function()
{
  .Call("cgraph", self, new.env(), PACKAGE = "cgraph")

  self$active()
}

#' Add Constant
#'
#' Add a constant node to the graph.
#'
#' @details \code{$const(value = NULL, name = NULL)}
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note Constants are ignored when differentiating a graph. The intended use of constants is that they are given a fixed value. However, it is still possible to change the value of constants when evaluating or differentiating a graph (see \link[cgraph]{cg_run} and \link[cgraph]{cg_gradients} for more details).
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
#' @name cg_const
#' @author Ron Triepels
cgraph$public_methods$const <- function(value = NULL, name = NULL)
{
  .Call("cg_add_constant", value, name, self, PACKAGE = "cgraph")
}

#' Add Input
#'
#' Add an input node to the graph.
#'
#' @details \code{$input(value = NULL, name = NULL)}
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note The intended use of inputs is that they are not given a fixed value but behave as placeholders. Values can be supplied for inputs when evaluating or differentiating a graph (see \link[cgraph]{cg_run} and \link[cgraph]{cg_gradients} for more details).
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
#' @name cg_input
#' @author Ron Triepels
cgraph$public_methods$input <- function(value = NULL, name = NULL)
{
  .Call("cg_add_input", value, name, self, PACKAGE = "cgraph")
}

#' Add Parameter
#'
#' Add a parameter node to the graph.
#'
#' @details \code{$parm(value = NULL, name = NULL)}
#'
#' @param value numeric vector or array, value of the node (optional).
#' @param name character scalar, name of the node (optional). In case \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note Parameters are assumed to be subject to some optimization process. Hence, their value might change over time.
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
#' @name cg_parm
#' @author Ron Triepels
cgraph$public_methods$parm <- function(value = NULL, name = NULL)
{
  .Call("cg_add_parameter", value, name, self, PACKAGE = "cgraph")
}

#' Get Parameters
#'
#' List all parameters and their values.
#'
#' @details \code{$get_parms()}
#'
#' @return named list, parameters of the graph.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add some parameters.
#' x$add_parms(prm1 = 1, prm2 = 2, prm3 = 3)
#'
#' # List the parameters.
#' x$get_parms()
#'
#' @name cg_get_parms
#' @author Ron Triepels
cgraph$public_methods$get_parms <- function()
{
  .Call("cg_get_parms", self, PACKAGE = "cgraph")
}

#' Add Parameters
#'
#' Add parameters to the graph.
#'
#' @details \code{$add_parms(..., parms = NULL)}
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
#' x$add_parms(prm1 = 1, prm2 = 2, prm3 = 3)
#'
#' # List the parameters.
#' x$get_parms()
#'
#' @name cg_add_parms
#' @author Ron Triepels
cgraph$public_methods$add_parms <- function(..., parms = NULL)
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
#' @details \code{$opr(call, grads, binding, name = NULL)}
#'
#' @param call symbol, operation performed by the node.
#' @param grads list of symbols, gradients functions of the input nodes that are consumed by the operation in argument \code{call}.
#' @param args list of cg_node objects, the nodes that are consumed by the operation in argument \code{call}.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is tried to be added to the graph under an auto-generated name.
#'
#' @note The operation to be performed by the node should be provided as a symbol to argument \code{call}. If this operation consumes any other nodes in the graph, then the gradient function of the operation with respect to these input nodes should be supplied as a symbol to argument \code{gradients}. These gradients must be a function of each input's gradient. A gradient function must be provided for each input node as specified by argument \code{args}.
#'
#' There is a wrapper function \link[cgraph]{opr} that calls this method on the current active graph.
#'
#' @return cg.node, operation.
#'
#' @name cg_opr
#' @author Ron Triepels
cgraph$public_methods$opr <- function(call, grads, args, name = NULL)
{
  .Call("cg_add_operation", call, grads, args, name, self, PACKAGE = "cgraph")
}

#' Change Active Graph
#'
#' Set the graph to be the active graph.
#'
#' @details \code{$active()}
#'
#' @note Any nodes that are created are automatically added to the active graph. This also applies to operations that are created by overloaded S3 functions that do not follow the cg_<name> naming convention (such as primitive inflix functions '+' and '-').
#'
#' Only one graph can be active at a time. The active graph can be changed by calling method \link[cgraph]{cg_active} on another cgraph object.
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
#' @name cg_active
#' @author Ron Triepels
cgraph$public_methods$active <- function()
{
  assign("graph", self, envir = session)
}

#' Evaluate a Node in the Graph
#'
#' Evaluate node \code{name} in the graph.
#'
#' @details \code{$val(name)}
#'
#' @param name character scalar, name of the node that is evaluated.
#'
#' @note The values of all nodes are cached for performance reasons. Only those nodes needed to compute node \code{name} and that have not yet been retrieved by \link[cgraph]{cg_val} are computed.
#'
#' @return R object, the value of the node.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add a parameter
#' a <- x$parm(20, name = "a")
#'
#' # Evaluate a
#' x$val(a)
#'
#' @name cg_val
#' @author Ron Triepels
cgraph$public_methods$val <- function(name)
{
  .Call("cg_get", name, self)
}

#' Change the Value of a Node in the Graph
#'
#' Change the value of node \code{name} in the graph.
#'
#' @details \code{$set(name, value)}
#'
#' @param name character scalar, name of the node that is changed.
#' @param value R object, new value of the node.
#'
#' @note The cached value of all nodes that directly or indirectly dependend on node \code{name} is removed. The value of these nodes will be re-computed the next time \link[cgraph]{cg_val} is called.
#'
#' @return nothing.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add a parameter
#' a <- x$parm(20, name = "a")
#'
#' # Change value of a
#' x$set(a, 40)
#'
#' # Evaluate a
#' x$val(a)
#'
#' @name cg_set
#' @author Ron Triepels
cgraph$public_methods$set <- function(name, value)
{
  invisible(.Call("cg_set", name, value, self))
}

#' Evaluate the Graph
#'
#' Evaluate node \code{name} in the graph.
#'
#' @details \code{$run(name, values = new.env())}
#'
#' @param name character scalar, name of the node that is evaluated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#'
#' @note All nodes required to compute node \code{name} must have a value or their value must be able to be computed at run-time. Nodes can be assigned a value when they are created or by calling method \link[cgraph]{cg_set}. Alternatively, argument \code{values} can be used to substitute values for nodes that do not have a value (e.g. inputs) or to fix their values.
#'
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. Values of operation nodes that are cached by function \link[cgraph]{cg_get} are ignored and re-computed. The values of all nodes that are computed are returned.
#'
#' There is a wrapper function \link[cgraph]{run} that calls this method on the current active graph.
#'
#' @return environment, the value of node \code{name} including the value of all ancestors of \code{name}.
#'
#' @examples # Initialize a new computational graph.
#' x <- cgraph$new()
#'
#' # Add an input.
#' a <- x$input(name = "a")
#'
#' # Square the input (i.e. b = a^2).
#' b <- cg_pow(a, x$const(2), name = "b")
#'
#' # Evaluate b at a = 2.
#' values <- x$run(b, list(a = 2))
#'
#' # Retrieve the value of b.
#' values$b
#'
#' @name cg_run
#' @author Ron Triepels
cgraph$public_methods$run <- function(name, values = new.env())
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
#' @details \code{$gradients(name, values = new.env(), index = 1)}
#'
#' @param name character scalar, name of the node that is differentiated.
#' @param values named list or environment, values that are subsituted for the nodes in the graph.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#'
#' @note All nodes required to compute node \code{name} must have a value, or their value must be able to be computed at run-time. The values of nodes can be obtained by first evaluating node \code{name} in the graph using function \link[cgraph]{cg_run}. The values obtained by this function for the nodes can then be supplied to argument \code{values}.
#'
#' Currently, the cgraph package can only differentiate scalar target nodes. In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is to be differentiated.
#'
#' The gradients of all ancestors or \code{name} are returned. Constant nodes are not differentiated and their gradients are not returned. The gradients have the same shape as the values of the nodes.
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
#' c <- cg_sin(a) + cg_cos(b) - cg_tan(a)
#'
#' # Differentiate the graph with respect to c.
#' grads <- x$gradients(c, x$run(c))
#'
#' # Retrieve the gradient of c with respect to a.
#' grads$a
#'
#' @name cg_gradients
#' @author Ron Triepels
cgraph$public_methods$gradients <- function(name, values = new.env(), index = 1)
{
  if(is.list(values))
  {
    values <- list2env(values)
  }

  .Call("cg_gradients", name, values, new.env(), index, self, PACKAGE = "cgraph")
}

#' Adjacency Matrix
#'
#' Retrieve the adjacency matrix of the graph.
#'
#' @details \code{$adj.mat()}
#'
#' @return numeric matrix, the adjacency matrix of the graph.
#'
#' @name cg_adj_mat
#' @author Ron Triepels
cgraph$public_methods$adj_mat <- function()
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
#' @name cg_plot
#' @author Ron Triepels
cgraph$public_methods$plot <- function(...)
{
  Rgraphviz::plot(new("graphAM", adjMat = self$adj_mat(), edgemode = "directed"), ...)
}

#' @export
plot.cgraph <- function(x, ...)
{
  x$plot(...)
}
