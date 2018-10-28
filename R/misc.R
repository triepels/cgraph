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

#' Sigmoid
#'
#' Evaluate the sigmoid function on \code{x} element-wise.
#'
#' @param x numeric vector or array, object on which the sigmoid function is evaluated.
#'
#' @note The sigmoid function is defined as \code{1 / (1 + exp(-x))}. The resulting sigmoid values are clamped within the range \code{[a, 1 - a]} where \code{a} evaluates to \code{.Machine$double.eps} to avoid numerical underflow.
#'
#' @return numeric vector or array, sigmoid values.
#'
#' @author Ron Triepels
#' @export
sigmoid <- function(x)
{
  .Call("sigmoid", x, PACKAGE = "cgraph")
}

#' Sigmoid
#'
#' Calculate \code{sigmoid(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
#' @export
cg_sigmoid <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sigmoid),
    grads = list(
      quote(sigmoid_grad)
    ),
    args = list(x)
  )
}

#' Sigmoid Gradient
#'
#' Calculate the gradient of \code{1 / (1 + exp(-x))} with respect to \code{x}.
#'
#' @param y numeric vector or array, value of \code{1 / (1 + exp(-x))}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
sigmoid_grad <- function(x, val, grad)
{
  grad * val * (1 - val)
}
