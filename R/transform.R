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

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is identical to \code{cg.as.numeric}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.as.double <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(as.double),
    grads = list(
      quote(as.double.grad)
    ),
    args = list(x)
  )
}

#' Coerce to a Numeric Vector Gradient
#'
#' Calculate the gradient of \code{as.numeric(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
as.double.grad <- function(x, val, grad)
{
  if(is.array(x))
  {
    array(grad, dim(x))
  }
  else
  {
    as.numeric(grad)
  }
}

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#' @param ... further arguments passed to or from other methods.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
as.double.cg.node <- function(x, name, ...)
{
  .Deprecated("cg.as.double")

  cgraph::cg.as.double(x, name)
}

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is identical to \code{cg.as.double}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.as.numeric <- function(x, name)
{
  cgraph::cg.as.double(x, name)
}


#### TO DO: Check description of cg.reshape



#' Reshape Array Dimensions
#'
#' Change the dimensions of array \code{x} to \code{dim}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param dim numeric scalar or vector, the dimensions of the new array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note The elements of \code{x} are re-arranged column-wise.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.reshape <- function(x, dim, name)
{
  cgraph::opr(name = name,
    call = quote(array),
    grads = list(x = quote(cg.reshape.grad(x, grad))),
    binding = list(x = x)
  )
}

#' Reshape Array Dimensions Gradient
#'
#' Calculate the gradient of \code{array(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.reshape.grad <- function(x, grad)
{
  if(is.array(x))
  {
    array(bsum(grad, length(x)), dim(x))
  }
  else
  {
    bsum(grad, length(x))
  }
}

#' Coerce to an Array
#'
#' Coerce \code{x} to an array with dimensions \code{dim}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param dim numeric scalar or vector, the dimensions of the new array.
#' @param name character scalar, name of the operation (optional).
#' @param ... further arguments passed to or from other methods.
#'
#' @note This function is equivalent to function \code{cg.reshape}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
as.array.cg.node <- function(x, dim, name = cgraph::name(), ...)
{
  .Deprecated("cg.reshape")

  cgraph::cg.reshape(x, dim, name)
}

#' Matrix Transpose
#'
#' Perform \code{t(x)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.t <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(t),
    grads = list(
      x = quote(t.grad)
    ),
    args = list(x)
  )
}

t.grad <- function(x, val, grad)
{
  t(grad)
}

#' Matrix Transpose
#'
#' Perform \code{t(x)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
t.cg.node <- function(x)
{
  .Deprecated("cg.t")

  cgraph::cg.t(x)
}
