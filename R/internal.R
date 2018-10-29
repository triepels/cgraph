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

#' Block Summation
#'
#' Divide a vector or array in consecutive blocks of \code{n} elements and sum the elements at each position in these blocks.
#'
#' @param x, numeric vector or array, the object that is summed.
#' @param n, numeric scalar, block size. Defaults to 1.
#'
#' @note If \code{x} is an array and \code{n} is equal to the size of \code{x}'s first dimension, then \link[cgraph]{bsum} behaves as \link[base:colSums]{rowSums}.
#'
#' @return numeric vector, a \code{n}-dimensional vector, where the 1th element of the vector is the sum of each 1th element of the blocks, the 2nd element of the vector is the sum of each 2nd element of the blocks, and so on.
#'
#' @author Ron Triepels
#' @keywords internal
bsum <- function(x, n = 1)
{
  .Call("bsum", x, n, PACKAGE = "cgraph")
}

#' Approximate Gradients
#'
#' Differentiate node \code{x} with respect to node \code{y} by numerical differentiation.
#'
#' @param x character scalar, name of the node.
#' @param y character scalar, name of the node.
#' @param index numeric scalar, index of the target node that is differentiated. Defaults to the first element.
#' @param eps numeric scalar, step size. Defaults to 1e-4.
#'
#' @note All nodes required to compute node \code{name} must have a value, or their value must be able to be computed at run-time.
#'
#' The graph is differentiation by the symmetric difference quotient. This method can only be used to differentiate scalars. In case the value of target node \code{name} is a vector or an array, argument \code{index} can be used to specify which element of the vector or array is differentiated. The caluclated gradient has the same shape as node \code{y}.
#'
#' Numerical differentiation is subject to estimation error and can be very slow. Therefore, this function should only be used for testing purposes.
#'
#' @return numeric vector or array, the derivative of \code{x} with respect to \code{y}.
#'
#' @author Ron Triepels
#' @keywords internal
approx_grad <- function(x, y, index = 1, eps = 1e-4)
{
  grad <- y.value <- val(y)

  for(i in 1:length(grad))
  {
    y.value[i] <- y.value[i] + eps

    set(y, y.value)

    x.value1 <- val(x)[index]

    y.value[i] <- y.value[i] - 2 * eps

    set(y, y.value)

    x.value2 <- val(x)[index]

    y.value[i] <- y.value[i] + eps

    set(y, y.value)

    grad[i] <- (x.value1 - x.value2) / (2 * eps)
  }

  grad
}
