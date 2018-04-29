#' Generate Name
#'
#' Generate a default name for an expression.
#'
#' @return symbol, auto-generated name for the expression.
#'
#' @author Ron Triepels
name <- function()
{
  if(!exists("graph", envir = .session))
  {
    stop("No current graph set")
  }

  .session$graph$name()
}

#' Add Constant
#'
#' Add a constant node to the current graph.
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
  if(!exists("graph", envir = .session))
  {
    stop("No current graph set")
  }

  .session$graph$const(value, name)
}

#' Add Input
#'
#' Add an input node to the current graph.
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @return cg.node, input node.
#'
#' @author Ron Triepels
input <- function(value, name)
{
  if(!exists("graph", envir = .session))
  {
    stop("No current graph set")
  }

  .session$graph$input(value, name)
}

#' Add Parameter
#'
#' Add a parameter node to the current graph.
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @return cg.node, parameter node.
#'
#' @author Ron Triepels
parm <- function(value, name)
{
  if(!exists("graph", envir = .session))
  {
    stop("No current graph set")
  }

  .session$graph$parm(value, name)
}

#' Add Expression
#'
#' Add an expression node to the current graph.
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
  if(!exists("graph", envir = .session))
  {
    stop("No current graph set")
  }

  .session$graph$expr(call, grads, binding, name)
}

#' Array Transposition
#'
#' Transpose the row and columns of an array while keeping its dimensions the same.
#'
#' @param a array, the array to be transposed.
#'
#' @note This is a wrapper function for \code{aperm(a, replace(1:length(dim(a)), 1:2, 2:1), resize = F)}.
#'
#' @return array, transposed array.
#'
#' @name perm
#' @author Ron Triepels
perm <- function(a)
{
  return(aperm(a, replace(1:length(dim(a)), 1:2, 2:1), resize = F))
}
