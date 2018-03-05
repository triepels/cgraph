#' Computational Graph Class Generator
#'
#' Generator class to create new cgraph objects.
#'
#' @details \code{cgraph}
#'
#' @name cgraph
#' @author Ron Triepels
NULL

cgraph <- R6Class(
  classname = "cgraph",
  portable = F,
  class = T,
  cloneable = T,
  public = list(
    nodes = NULL,
    values = NULL,
    grad = NULL
  )
)

# Data of current graph.
.cg <- new.env()

#' Computational Graph
#'
#' Initialize a new computational graph.
#'
#' @details \code{$new()}
#'
#' @return cgraph object.
#'
#' @name cgraph.initialize
#' @author Ron Triepels
cgraph$public_methods$initialize <- function()
{
  pkg.env <- as.environment("package:cgraph")

  grad <- new.env(parent = pkg.env)

  values <- new.env(parent = grad)

  assign("graph", .Call("cgraph", self, values, grad), envir = pkg.env$.cg)
}

#' Generate Name
#'
#' Generate a default name for a node.
#'
#' @details \code{$name(type = 3)}
#'
#' @param type numeric scalar, type of the node. Should be either: 0 (constant), 1 (input), 2 (parameter), or 3 (expression). Defaults to 3 (expression).
#'
#' @return symbol, auto-generated name for the node.
#'
#' @name cgraph.name
#' @author Ron Triepels
cgraph$public_methods$name <- function(type = 3)
{
  type <- as.integer(type)

  .Call("cg_gen_name", type, self)
}

#' Count Nodes
#'
#' Count how many nodes of a specific type are added to the graph.
#'
#' @details \code{$count.type(type = 3)}
#'
#' @param type numeric scalar, type of the node. Should be either: 0 (constant), 1 (input), 2 (parameter), or 3 (expression). Defaults to 3 (expression).
#'
#' @return numeric scalar, the numer of nodes of the given type.
#'
#' @name cgraph.count.type
#' @author Ron Triepels
cgraph$public_methods$count.type <- function(type = 3)
{
  type <- as.integer(type)

  .Call("cg_count_type", type, self)
}

#' Add Constant
#'
#' Add a constant node to the graph.
#'
#' @details \code{$const(value, name)}
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @note Constant nodes are ignored when differentiating a graph.
#'
#' @return cg.node, constant node.
#'
#' @name cgraph.const
#' @author Ron Triepels
cgraph$public_methods$const <- function(value, name)
{
  type <- as.integer(0)

  if(missing(value)) value <- NULL

  if(!is.null(value) & !(is.numeric(value) | is.array(value)))
    stop("value must be a numeric vector or array")

  if(missing(name)) name <- as.character(self$name(type)) else name <- as.character(name)

  .Call("cg_add_placeholder", value, name, type, self)
}

#' Add Input
#'
#' Add an input node to the graph.
#'
#' @details \code{$input(value, name)}
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @return cg.node, input node.
#'
#' @name cgraph.input
#' @author Ron Triepels
cgraph$public_methods$input <- function(value, name)
{
  type <- as.integer(1)

  if(missing(value)) value <- NULL

  if(!is.null(value) & !(is.numeric(value) | is.array(value)))
    stop("value must be a numeric vector or array")

  if(missing(name)) name <- as.character(self$name(type)) else name <- as.character(name)

  .Call("cg_add_placeholder", value, name, type, self)
}

#' Add Parameter
#'
#' Add a parameter node to the graph.
#'
#' @details \code{$parm(value, name)}
#'
#' @param value numeric scalar or array, default value of the node.
#' @param name character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.
#'
#' @return cg.node, parameter node.
#'
#' @name cgraph.parm
#' @author Ron Triepels
cgraph$public_methods$parm <- function(value, name)
{
  type <- as.integer(2)

  if(missing(value)) value <- NULL

  if(!is.null(value) & !(is.numeric(value) | is.array(value)))
    stop("value must be a numeric vector or array")

  if(missing(name)) name <- as.character(self$name(type)) else name <- as.character(name)

  .Call("cg_add_placeholder", value, name, type, self)
}

#' Add Expression
#'
#' Add an expression node to the graph.
#'
#' @details \code{$expr(call, grads, binding, name)}
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
#' @name cgraph.expr
#' @author Ron Triepels
cgraph$public_methods$expr <- function(call, grads, binding, name)
{
  type <- as.integer(3)

  if(!(is.name(call) || is.call(call))) call <- as.call(call)

  grads <- as.list(grads)

  if(!is.environment(binding)) binding <- list2env(binding)

  if(missing(name)) name <- as.character(self$name(type)) else name <- as.character(name)

  .Call("cg_add_expression", call, grads, binding, name, self)
}

#' Evaluate a Graph
#'
#' Evaluate node \code{name} in the graph.
#'
#' @details \code{$run(name, values = list())}
#'
#' @param name character scalar or symbol, name of the node that needs to be evaluated.
#' @param values named list or environment, values that are subsituted for the placeholders in the graph.
#'
#' @note All placeholders required to compute node \code{name} must have a value. Placeholders can be assigned a default value when they are created. Alternatively, argument \code{values} can be used to substitute values for placeholders that do not have a default value or to fix the values of nodes.
#'
#' In case the return value of an expression has no \code{dim} attribute attached to it, the attribute is automatically attached. The only exception to this rule is if the expression casts the results explicitly to a numeric vector by function \code{c}.
#'
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. The values of placeholders whose default values are not changed are not returned.
#'
#' @return cg.results object, the value of node \code{name} including the values of all ancestors of node \code{name} that are evaluated in the forward-pass.
#'
#' @name cgraph.run
#' @author Ron Triepels
cgraph$public_methods$run <- function(name, values = list())
{
  name <- as.character(name)

  if (!is.environment(values)) values <- list2env(values, parent = self$values)

  .Call("cg_run", name, values, self)
}

#' Calculate Gradients
#'
#' Differentiate the graph with respect to node \code{name} by reverse automatic differentiation.
#'
#' @details \code{$gradients(name, values, index = 1)}
#'
#' @param name character scalar or symbol, name of the node that needs to be differentiated.
#' @param values named list or environment, values that are subsituted for the expressions and placeholders in the graph.
#' @param index numeric scalar, index of the target node that needs to be differentiated. Defaults to the first element.
#'
#' @note All placeholders and expressions required to compute node \code{name} must have a value. By default, expression nodes are unevaluated. The values of these nodes can be obtained by evaluating the graph using function \code{run()}. The values obtained by this function for the expression nodes can be supplied along values for the placeholders via argument \code{values}.
#'
#' In case the value of node \code{name} is an array, \code{index} can be used to specify which element of the array needs to be differentiated.
#'
#' The gradients of all parameters are returned along with the gradients of all intermediate nodes that are differentiated in the backward-pass. Constant nodes are not differentiated and their gradients are not returned. Moreover, the gradients of parameters have the same shape as the parameters themselves.
#'
#' @return cg.results object, the gradients of all nodes evaluated in the backward-pass with respect to node \code{name}.
#'
#' @name cgraph.gradients
#' @author Ron Triepels
cgraph$public_methods$gradients <- function(name, values = list(), index = 1)
{
  grads = new.env()

  name <- as.character(name)

  index <- as.integer(index)

  if (!is.environment(values)) values <- list2env(values, parent = self$values)

  .Call("cg_gradients", name, index, values, grads, self)
}

#' Adjacency Matrix
#'
#' Create an adjacency matrix of the computational graph.
#'
#' @details \code{$adj.mat()}
#'
#' @return numeric matrix, the adjacency matrix of the graph.
#'
#' @name cgraph.adj.mat
#' @author Ron Triepels
cgraph$public_methods$adj.mat <- function()
{
  .Call("cg_adj_mat", self)
}

#' Plot
#'
#' Plot the topology of the computational graph.
#'
#' @details \code{$plot(...)}
#'
#' @param ... additional arguments that can be passed on to the plot function of the Rgraphiz package.
#'
#' @note A visual representation of the computational graph might be usefull for debugging purposes. This functions requires the Rgraphviz package.
#'
#' @return none.
#'
#' @name cgraph.plot
#' @author Ron Triepels
cgraph$public_methods$plot <- function(...)
{
  Rgraphviz::plot(new("graphAM", adjMat = self$adj.mat(), edgemode = "directed"), ...)
}
