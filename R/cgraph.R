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
  class = T,
  cloneable = T,
  public = list(
    nodes = NULL,
    values = NULL
  )
)

#' Computational Graph
#'
#' Initialize a new computational graph.
#'
#' @details \code{$new()}
#'
#' @note The cgraph object is set as the current graph. Any placeholders or expressions that are invoked will be added to this graph. You can change the current graph by calling the \code{$active()} method on another cgraph object.
#'
#' @return cgraph object.
#'
#' @name cg.initialize
#' @author Ron Triepels
cgraph$public_methods$initialize <- function()
{
  values <- new.env(parent = session$functions)

  graph <- .Call("cgraph", self, values, PACKAGE = "cgraph")

  self$active()
}

#' Generate Name
#'
#' Generate a default name for a node.
#'
#' @details \code{$name(type = 3)}
#'
#' @param type numeric scalar, type of the node. Should be either: 0 (constant), 1 (input), 2 (parameter), or 3 (expression). Defaults to 3 (expression).
#'
#' @note The auto-generated name is not guaranteed to be unique. Instead, it may alread be used by a node in the graph.
#'
#' @return symbol, auto-generated name for the node.
#'
#' @name cg.name
#' @author Ron Triepels
cgraph$public_methods$name <- function(type = 3)
{
  type <- as.integer(type)

  .Call("cg_gen_name", type, self, PACKAGE = "cgraph")
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
#' @name cg.const
#' @author Ron Triepels
cgraph$public_methods$const <- function(value, name)
{
  type <- as.integer(0)

  if(missing(value))
  {
    value <- NULL
  }
  else
  {
    if(!(is.numeric(value) | is.array(value)))
    {
      stop("value must be a numeric vector or array")
    }
  }

  if(missing(name))
  {
    name <- as.character(self$name(type))
  }
  else
  {
    name <- as.character(name)
  }

  .Call("cg_add_placeholder", value, name, type, self, PACKAGE = "cgraph")
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
#' @name cg.input
#' @author Ron Triepels
cgraph$public_methods$input <- function(value, name)
{
  type <- as.integer(1)

  if(missing(value))
  {
    value <- NULL
  }
  else
  {
    if(!(is.numeric(value) | is.array(value)))
    {
      stop("value must be a numeric vector or array")
    }
  }

  if(missing(name))
  {
    name <- as.character(self$name(type))
  }
  else
  {
    name <- as.character(name)
  }

  .Call("cg_add_placeholder", value, name, type, self, PACKAGE = "cgraph")
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
#' @name cg.parm
#' @author Ron Triepels
cgraph$public_methods$parm <- function(value, name)
{
  type <- as.integer(2)

  if(missing(value))
  {
    value <- NULL
  }
  else
  {
    if(!(is.numeric(value) | is.array(value)))
    {
      stop("value must be a numeric vector or array")
    }
  }

  if(missing(name))
  {
    name <- as.character(self$name(type))
  }
  else
  {
    name <- as.character(name)
  }

  .Call("cg_add_placeholder", value, name, type, self, PACKAGE = "cgraph")
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
#' @name cg.expr
#' @author Ron Triepels
cgraph$public_methods$expr <- function(call, grads, binding, name)
{
  type <- as.integer(3)

  if(!(is.name(call) || is.call(call)))
  {
    call <- as.call(call)
  }

  grads <- as.list(grads)

  if(!is.environment(binding))
  {
    if(!is.list(binding))
    {
      stop("binding must be a named list or environment")
    }

    binding <- list2env(binding)
  }

  if(missing(name))
  {
    name <- as.character(self$name(type))
  }
  else
  {
    name <- as.character(name)
  }

  .Call("cg_add_expression", call, grads, binding, name, self, PACKAGE = "cgraph")
}

#' Change the Default Value of a Node
#'
#' Change the default value of a node in the graph.
#'
#' @details \code{$set(name. value)}
#'
#' @param name cg.node, name of the node whose default value is to be changed.
#' @param value numeric vector or array, default value of the node.
#'
#' @return nothing.
#'
#' @name cg.set
#' @author Ron Triepels
cgraph$public_methods$set <- function(name, value)
{
  name <- as.character(name)

  if(!(is.numeric(value) | is.array(value)))
  {
    stop("value must be a numeric vector or array")
  }

  .Call("cg_set", name, value, self, PACKAGE = "cgraph")

  invisible()
}

#' Change active graph
#'
#' Set this graph to be the active graph.
#'
#' @details \code{$active()}
#'
#' @note Any placeholders or expressions that are invoked are added to the active graph. This behavior also applies to expressions that are invoked by overloaded S3 functions that do not follow the cg.<name> naming convention (such as primitive functions '+' and '-'). Moreover, only one graph can be active at a time. You can use this function to change the active graph.
#'
#' @return none.
#'
#' @name cg.active
#' @author Ron Triepels
cgraph$public_methods$active <- function()
{
  assign("graph", self, envir = session)
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
#' Only those nodes needed to compute node \code{name} are evaluated and their values are returned. The values of placeholders whose default values are not changed are not returned.
#'
#' @return cg.environment object, the value of node \code{name} including the values of all ancestors of node \code{name} that are evaluated in the forward-pass.
#'
#' @name cg.run
#' @author Ron Triepels
cgraph$public_methods$run <- function(name, values = list())
{
  name <- as.character(name)

  if(!is.environment(values))
  {
    if(!is.list(values))
    {
      stop("values must be a named list or environment")
    }

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
#' @name cg.gradients
#' @author Ron Triepels
cgraph$public_methods$gradients <- function(name, values = list(), index = 1)
{
  name <- as.character(name)

  index <- as.integer(index)

  if(!is.environment(values))
  {
    if(!is.list(values))
    {
      stop("values must be a named list or environment")
    }

    values <- list2env(values)
  }

  .Call("cg_gradients", name, index, values, self, PACKAGE = "cgraph")
}


#' Approximate Gradients
#'
#' Differentiate node \code{x} with respect to node \code{y} in the graph by numerical differentiation.
#'
#' @details \code{$approx.grad(x, y, values = list(), index = 1, eps = 1e-4)}
#'
#' @param x character scalar or symbol, name of the node.
#' @param y character scalar or symbol, name of the node.
#' @param values named list or environment, values that are subsituted for the expressions and placeholders in the graph.
#' @param index numeric scalar, index of the target node that needs to be differentiated. Defaults to the first element.
#' @param eps numeric scalar, step size. Defaults to 1e-4.
#'
#' @note All placeholders required to compute node \code{name} must have a value. Placeholders can be assigned a default value when they are created. Alternatively, argument \code{values} can be used to substitute values for placeholders that do not have a default value or to fix the values of nodes.
#'
#' The graph is differentiation by the symmetric difference quotient. This function is mainly used for testing purposes.
#'
#' @return numeric scalar or array, the derivative of \code{x} with respect to \code{y}.
#'
#' @name cg.approx.grad
#' @author Ron Triepels
cgraph$public_methods$approx.grad <- function(x, y, values = list(), index = 1, eps = 1e-4)
{
  x <- as.character(x)

  y <- as.character(y)

  index <- as.integer(index)

  eps <- as.numeric(eps)

  if(!is.environment(values))
  {
    if(!is.list(values))
    {
      stop("values must be a named list or environment")
    }

    values <- list2env(values)
  }

  .Call("cg_approx_grad", x, y, index, values, eps, self, PACKAGE = "cgraph")
}

#' Adjacency Matrix
#'
#' Create an adjacency matrix of the computational graph.
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
#' @name cg.plot
#' @author Ron Triepels
cgraph$public_methods$plot <- function(...)
{
  Rgraphviz::plot(new("graphAM", adjMat = self$adj.mat(), edgemode = "directed"), ...)
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
