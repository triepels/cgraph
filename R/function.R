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

#' Create function
#'
#' Initialize a new function that can be used by operators in a graph.
#'
#' @param def function, the definition of the function.
#' @param grads list of functions, the gradient functions with respect to each input (optional).
#'
#' @note If the function consumes any inputs, then the gradient function with respect to these inputs must be provided to argument \code{grads}. These gradients must be a function of each input's gradient and take as arguments the inputs of the function including argument \code{val} and \code{grad}. These latter two arguments evaluate to the value of the function and its gradient respectively at run-time.
#'
#' @return cg_function object.
#'
#' @examples #' # Create a custom negation function
#' f <- cg_function(
#'     def = function(x) -x,
#'     grads = list(function(x, val, grad) -grad)
#' )
#'
#' @export
#' @author Ron Triepels
cg_function <- function(def, grads = list())
{
  .Call("cg_function", def, grads, PACKAGE = "cgraph")
}

#' @export
print.cg_function <- function(x, ...)
{
  cat("<cg_function>\n")
}
