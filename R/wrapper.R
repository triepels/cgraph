#' Generate Name
#'
#' Generate a default name for an expression.
#'
#' @note The auto-generated name is not guaranteed to be unique. Instead, it may alread be used by an expression node in the active graph.
#'
#' @return symbol, auto-generated name for the node.
#'
#' @author Ron Triepels
name <- function()
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$name()
}

#' Add Constant
#'
#' Add a constant node to the active graph.
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @note Constant nodes are ignored when differentiating a graph.
#'
#' @return cg.node, constant node.
#'
#' @author Ron Triepels
const <- function(value, name)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$const(value, name)
}

#' Add Input
#'
#' Add an input node to the active graph.
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @return cg.node, input node.
#'
#' @author Ron Triepels
input <- function(value, name)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$input(value, name)
}

#' Add Parameter
#'
#' Add a parameter node to the active graph.
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @return cg.node, parameter node.
#'
#' @author Ron Triepels
parm <- function(value, name)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$parm(value, name)
}

#' Add Expression
#'
#' Add an expression node to the active graph.
#'
#' @param call expression or call, operations performed by the node.
#' @param grads named list of expressions or calls, gradients of the inputs and parameters used in \code{call} with respect to the node.
#' @param binding named list or environment, binds the values used in the expressions or calls of \code{call} and \code{grads} to the symbols of the nodes in the graph.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @note The operation that is to be performed by the node should be provided as an expression or call to argument \code{call}. If this operation contains any inputs or parameters, then the gradient of these inputs and parameters with respect to the node should be provided via argument \code{gradients}. Also, any variables used in the these expressions or calls should be bind to the symbols of the nodes in the graph. There are two ways to bind variables. Either, \code{binding} should be a named list were the names of nodes are assigned as symbols to the named members, or \code{binding} is an environment were the names of nodes are assigned as symbols to objects within the environment.
#'
#' @return cg.node, expression node.
#'
#' @author Ron Triepels
expr <- function(call, grads, binding, name)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$expr(call, grads, binding, name)
}

#' Change the Default Value of a Node
#'
#' Change the default value of a node in the active graph.
#'
#' @param name cg.node, name of the node whose default value is to be changed.
#' @param value numeric vector or array, default value of the node.
#'
#' @return nothing.
#'
#' @author Ron Triepels
set <- function(name, value)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$set(name, value)
}

#' Change the Default Value of a Node
#'
#' Change the default value of a node in the active graph.
#'
#' @param name cg.node, name of the node whose default value is to be changed.
#' @param value numeric vector or array, default value of the node.
#'
#' @return nothing.
#'
#' @author Ron Triepels
`%:%` <- function(name, value)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$set(name, value)
}

#' Evaluate a Graph
#'
#' Evaluate node \code{name} in the active graph.
#'
#' @param name character scalar or symbol, name of the node that needs to be evaluated.
#' @param values named list or environment, values that are subsituted for the placeholders in the graph.
#'
#' @note All placeholders required to compute node \code{name} must have a value. Placeholders can be assigned a default value when they are created. Alternatively, argument \code{values} can be used to substitute values for placeholders that do not have a default value or to fix the values of nodes.
#'
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. The values of placeholders whose default values are not changed are not returned.
#'
#' @return cg.environment object, the value of node \code{name} including the values of all ancestors of node \code{name} that are evaluated in the forward-pass.
#'
#' @author Ron Triepels
run <- function(name, values = list())
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$run(name, values)
}

#' Calculate Gradients
#'
#' Differentiate the active graph with respect to node \code{name} by reverse automatic differentiation.
#'
#' @param name character scalar or symbol, name of the node that needs to be differentiated.
#' @param values named list or environment, values that are subsituted for the expressions and placeholders in the graph.
#' @param index numeric scalar, index of the target node that needs to be differentiated. Defaults to the first element.
#'
#' @note All placeholders and expressions required to compute node \code{name} must have a value. By default, expression nodes are unevaluated. The values of these nodes can be obtained by evaluating the graph using function \code{$run()}. The values obtained by this function for the expression nodes can be supplied along values for the placeholders via argument \code{values}.
#'
#' Currently, cgraph can only differentiate with respect to a scalar output node. In case the value of output node \code{name} is a vector or an array, \code{index} can be used to specify which element of the vector or array needs to be differentiated.
#'
#' The gradients of all parameters are returned along with the gradients of all ancestor nodes of node \code{name} that are differentiated in the backward-pass. Constant nodes are not differentiated and their gradients are not returned. Moreover, the gradients of parameters have the same shape as the parameters themselves.
#'
#' @return cg.environment object, the gradients of all nodes evaluated in the backward-pass with respect to node \code{name}.
#'
#' @author Ron Triepels
gradients <- function(name, values = list(), index = 1)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$gradients(name, values, index)
}

#' Approximate Gradients
#'
#' Differentiate node \code{x} with respect to node \code{y} in the active graph by numerical differentiation.
#'
#' @param x character scalar or symbol, name of the node.
#' @param y character scalar or symbol, name of the node.
#' @param values named list or environment, values that are subsituted for the expressions and placeholders in the graph.
#' @param index numeric scalar, index of the target node that needs to be differentiated. Defaults to the first element.
#' @param eps numeric scalar, step size. Defaults to 1e-4.
#'
#' @note All placeholders and expressions required to compute node \code{name} must have a value. By default, expression nodes are unevaluated. The values of these nodes can be obtained by evaluating the graph using function \code{$run()}. The values obtained by this function for the expression nodes can be supplied along values for the placeholders via argument \code{values}.
#'
#' The graph is differentiation by the symmetric difference quotient. This function is mainly used for testing purposes.
#'
#' @return numeric scalar or array, the derivative of \code{x} with respect to \code{y}.
#'
#' @author Ron Triepels
approx.grad <- function(x, y, values = list(), index = 1, eps = 1e-4)
{
  if(!exists("graph", envir = .cg))
  {
    stop("No active graph set")
  }

  .cg$graph$approx.grad(x, y, values, index, eps)
}

#' Plot
#'
#' Plot the topology of a computational graph.
#'
#' @param x cgraph object, computational graph that is to be plotted.
#' @param ... additional arguments that can be passed on to the plot function of the Rgraphiz package.
#'
#' @note A visual representation of the computational graph might be usefull for debugging purposes. This functions requires the Rgraphviz package.
#'
#' @return none.
#'
#' @author Ron Triepels
plot.cgraph <- function(x, ...)
{
  x$plot(...)
}
