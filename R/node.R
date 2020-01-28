# Copyright 2020 Ron Triepels
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

#' Add Constant
#'
#' Add a constant node to the active graph.
#'
#' @param value R object, value of the node.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Constant nodes are ignored when differentiating a graph.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
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
#' @param value numerical vector or array, value of the node.
#' @param name character scalar, name of the node (optional). In case argument \code{name} is missing, the node is added to the graph under an automatically generated name.
#'
#' @note Parameters are assumed to be subject to some optimization process. Hence, their value might change over time. You can use data member \code{value} of a cg_node object to retrieve or change its value.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
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
#' @note Inputs cannot be assigned a value upon creation. Instead, they behave as placeholders. You can use data member \code{value} of a cg_node object to retrieve or change its value.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
#'
#' # Add an input with name 'a' to the graph.
#' a <- cg_input(name = "a")
#'
#' # Set the value to 2
#' a$value <- 2
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
#' @note Any objects that are supplied to argument \code{inputs} that are not cg_node objects are implicitly coerced to cg_constant objects.
#'
#' The elements of argument \code{input} can be named to control how the arguments of the function provided to argument \code{fun} are machted when the function is evaluated. In case no names are provided, arguments are matched positionally.
#'
#' @return cg_node object.
#'
#' @examples # Initialize a computational graph
#' graph <- cg_graph()
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
