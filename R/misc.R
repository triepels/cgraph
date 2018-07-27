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
#' Calculate \code{1 / (1 + exp(-x))}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sigmoid <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(1 / (1 + exp(-x))),
    grads = list(x = quote(cg.sigmoid.grad(y, grad))),
    binding = list(x = x, y = name)
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
cg.sigmoid.grad <- function(y, grad)
{
  grad * y * (1 - y)
}
