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
  .Call("cg_constant", value, name, PACKAGE = "cgraph")
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
  .Call("cg_parameter", value, name, PACKAGE = "cgraph")
}

#' Add Input
#'
#' Add an input node to the active graph.
#'
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Inputs cannot be assigned a fixed value but behave as placeholders. Values can be assigned to inputs when evaluating or differentiating a graph (see \link[cgraph]{cg_graph_run} and \link[cgraph]{cg_graph_gradients} for more details).
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
  .Call("cg_input", name, PACKAGE = "cgraph")
}

#' Add Operator
#'
#' Add an operation node to the active graph.
#'
#' @param fun cg_function object, function evaluated by the node.
#' @param inputs list, the nodes that are consumed by the operation.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Any objects that are supplied to argument \code{inputs} that are not cg_node objects are implicitly converted to cg_constant objects.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Create a custom negation function
#' f <- cg_function(
#'     def = function(x) -x,
#'     grads = list(function(x, val, grad) -grad)
#' )
#'
#' # Add a an operator with the negation function to the graph.
#' a <- cg_operator(f, list(10), name = "a")
#'
#' @author Ron Triepels
#' @export
cg_operator <- function(fun, inputs, name = NULL)
{
  .Call("cg_operator", fun, inputs, name, PACKAGE = "cgraph")
}

#' @export
print.cg_node <- function(x, ...)
{
  cat(sprintf("<cg_node: %s>\n", x$name))
}
