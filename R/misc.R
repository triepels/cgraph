# Copyright (C) 2018 Ron Triepels
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
