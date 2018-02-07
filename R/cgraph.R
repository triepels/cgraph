#' Computational Graph Class Generator
#'
#' Generator class to create new cgraph objects.
#'
#' @section Usage:
#' \preformatted{cgraph}
#'
#' @section Methods:
#' \describe{
#' \item{\code{$new()}}{initialize a new computational graph.}
#' }
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

#' Computational Graph
#'
#' Initialize a new computational graph.
#'
#' @section Usage:
#' \preformatted{cgraph$new()}
#'
#' @return cgraph object.
#'
#' @name initialize
#' @author Ron Triepels
cgraph$public_methods$initialize <- function()
{
  grad <- new.env(parent = as.environment("package:cgraph"))

  values <- new.env(parent = grad)

  .Call("cgraph", self, values, grad)
}

#' Generate Name
#'
#' Generate a default name for a node.
#'
#' @section Usage:
#' \preformatted{name(type)}
#'
#' @section Agruments:
#' \describe{
#' \item{type}{numeric scalar, type of the node. Should be either: 0 (constant), 1 (input), 2 (parameter), or 3 (expression). Defaults to 3 (expression).}
#' }
#'
#' @return symbol, auto-generated name for the node.
#'
#' @name name
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
#' @section Usage:
#' \preformatted{count.type(type)}
#'
#' @section Agruments:
#' \describe{
#' \item{type}{numeric scalar, type of the node. Should be either: 0 (constant), 1 (input), 2 (parameter), or 3 (expression). Defaults to 3 (expression).}
#' }
#'
#' @return numeric scalar, the numer of nodes of the given type.
#'
#' @name count.type
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
#' @section Usage:
#' \preformatted{const(value, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{value}{numeric scalar or array, default value of the node.}
#' \item{name}{character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.}
#' }
#'
#' @note Constant nodes are ignored when differentiating a graph.
#'
#' @return symbol, the name of the node.
#'
#' @name const
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
#' @section Usage:
#' \preformatted{input(value, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{value}{numeric scalar or array, default value of the node.}
#' \item{name}{character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.}
#' }
#'
#' @return symbol, the name of the node.
#'
#' @name input
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
#' @section Usage:
#' \preformatted{parm(value, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{value}{numeric scalar or array, default value of the node.}
#' \item{name}{character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.}
#' }
#'
#' @return symbol, the name of the node.
#'
#' @name parm
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
#' @section Usage:
#' \preformatted{expr(call, grads, binding, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{call}{expression or call, operations performed by the node.}
#' \item{grads}{named list of expressions or calls, gradients of the inputs and parameters used in \code{call} with respect to the node.}
#' \item{binding}{named list or environment, binds the values used in the expressions or calls of \code{call} and \code{grads} to the symbols of the nodes in the graph.}
#' \item{name}{character scalar or symbol, name of the node (optional). In case \code{name} is missing, an auto-generated name is assigned to the node.}
#' }
#'
#' @note The operation that is to be performed by the node should be provided as an expression or call to argument \code{call}. If this operation contains any inputs or parameters, then the gradient of these inputs and parameters with respect to the node should be provided via argument \code{gradients}. Also, any variables used in the these expressions or calls should be bind to the symbols of the nodes in the graph. There are two ways to bind variables. Either, \code{binding} should be a named list were the names of nodes are assigned as symbols to the named members, or \code{binding} is an environment were the names of nodes are assigned as symbols to objects within the environment.
#'
#' @return symbol, the name of the node.
#'
#' @name expr
#' @author Ron Triepels
cgraph$public_methods$expr <- function(call, grads, binding, name)
{
  call <- as.call(call)

  grads <- as.list(grads)

  if(!is.environment(binding)) binding <- list2env(binding)

  if(missing(name)) name <- as.character(self$name(type)) else name <- as.character(name)

  .Call("cg_add_expression", call, grads, binding, name, self)
}

#' Execute Graph
#'
#' Evaluate operations in the graph.
#'
#' @section Usage:
#' \preformatted{run(name, values)}
#'
#' @section Agruments:
#' \describe{
#' \item{name}{character scalar or symbol, name of the node that needs to be evaluated.}
#' \item{values}{named list or environment, values that are subsituted for the placeholders in the graph.}
#' }
#'
#' @note All placeholders required to compute node \code{name} must have a value. Placeholders can be assigned a default value when they are created. Alternatively, argument \code{values} can be used to substitute values for placeholders that do not have a default value or to fix the values of nodes.
#'
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. The values of placeholders whose default values are not changed are not returned.
#'
#' @return cg.results object, the values of all nodes executed in the forward-pass.
#'
#' @name run
#' @author Ron Triepels
cgraph$public_methods$run <- function(name, values = list())
{
  name <- as.character(name)

  if (!is.environment(values)) values <- list2env(values, parent = self$values)

  .Call("cg_run", name, values, self)
}

#' Calculate Gradients
#'
#' Differentiate nodes by reverse automatic differentiation
#'
#' @section Usage:
#' \preformatted{gradients(name, values)}
#'
#' @section Agruments:
#' \describe{
#' \item{name}{character scalar or symbol, name of the node that needs to be differentiated.}
#' \item{values}{named list or environment, values that are subsituted for the placeholders in the graph.}
#' }
#'
#' @note All placeholders and expressions required to compute node \code{name} must have a value. By default, expression nodes are unevaluated. The values of these nodes can be obtained by executing the graph using function \code{run()}. The values obtained by this function for the expressions in the graph can be supplied along values for the placeholders via argument \code{values}.
#'
#' The gradients of all parameters are returned along with the gradients of all intermediate nodes that are differentiated in the backward-pass. Constant nodes are not differentiated and their gradients are not returned.
#'
#' @return cg.results object, the gradients of all nodes calculated in the backward-pass.
#'
#' @name gradients
#' @author Ron Triepels
cgraph$public_methods$gradients <- function(name, values = list())
{
  grads = new.env()

  name <- as.character(name)

  if (!is.environment(values)) values <- list2env(values, parent = self$values)

  .Call("cg_gradients", name, values, grads, self)
}

#' Adjacency Matrix
#'
#' Create an adjacency matrix of the computational graph.
#'
#' @section Usage:
#' \preformatted{adj.mat(value, name)}
#'
#' @return numeric matrix, the adjacency matrix of the graph.
#'
#' @name adj.mat
#' @author Ron Triepels
cgraph$public_methods$adj.mat <- function()
{
  .Call("cg_adj_mat", self)
}

#' Plot
#'
#' Plot the topology of the computational graph.
#'
#' @section Usage:
#' \preformatted{plot(...)}
#'
#' @section Agruments:
#' \describe{
#' \item{...}{additional arguments that can be passed on to the plot function of the Rgraphiz package.}
#' }
#'
#' @note A visual representation of the computational graph might be usefull for debugging purposes. This functions requires the Rgraphviz package.
#'
#' @return none.
#'
#' @name plot
#' @author Ron Triepels
cgraph$public_methods$plot <- function(...)
{
  Rgraphviz::plot(new("graphAM", adjMat = self$adj.mat(), edgemode = "directed"), ...)
}
