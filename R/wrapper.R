#' Assign a Value to a Node
#'
#' Assign a default value to a node in the current graph.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param value numeric scalar or array, default value of the node.
#'
#' @return nothing.
#'
#' @author Ron Triepels
`%:%` <- function(x, value)
{
  x <- as.character(x)

  if(!exists("graph", envir = .cg))
  {
    stop("No current graph set")
  }

  assign(x, value, envir = .cg$graph$values)
}

#' Generate Name
#'
#' Generate a default name for an expression.
#'
#' @return symbol, auto-generated name for the expression.
#'
#' @author Ron Triepels
name <- function()
{
  if(!exists("graph", envir = .cg))
  {
    stop("No current graph set")
  }

  .cg$graph$name()
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
  if(!exists("graph", envir = .cg))
  {
    stop("No current graph set")
  }

  .cg$graph$const(value, name)
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
  if(!exists("graph", envir = .cg))
  {
    stop("No current graph set")
  }

  .cg$graph$input(value, name)
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
  if(!exists("graph", envir = .cg))
  {
    stop("No current graph set")
  }

  .cg$graph$parm(value, name)
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
  if(!exists("graph", envir = .cg))
  {
    stop("No current graph set")
  }

  .cg$graph$expr(call, grads, binding, name)
}

#' Dimensions of an Vector or Array
#'
#' Retrieve the dimension of an Vector or Array
#'
#' @param x vector or array, the object whose dimensions need to be retrieved.
#'
#' @note If \code{x} is an array, the function will return \code{dim(x)}. Otherwise, the function returns \code{length(x)}.
#'
#' @return numeric scalar, the dimensions of the vector or array.
#'
#' @author Ron Triepels
d <- function(x)
{
  if(is.array(x))
  {
    return(dim(x))
  }
  else
  {
    return(length(x))
  }
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
