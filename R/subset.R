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

#' Subset
#'
#' Calculate \code{x[...]}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param ... either cg_node objects or a numerical scalars that are passed on to the \code{`[`} function.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable with respect to the arguments provided to \code{...}. Any attempt to differentiate this operator with respect to these arguments results in an error.
#'
#' @seealso \link[base:Extract]{subset}
#'
#' @author Ron Triepels
#' @export
cg_subset1 <- function(x, ..., name = NULL)
{
  cg_operator(subset1, c(x, dots()))
}

# Function definition
delayedAssign("subset1", cg_function(
  def = base::`[`,
  grads = list(
    function(x, ..., drop = TRUE, value, grad)
    {
      if(!is.numeric(x))
      {
        stop(sprintf("cannot differentiate object of type '%s'", typeof(x)))
      }

      if(is.array(x))
      {
        out <- array(0, dim(x))
        out[...] <- grad
        out
      }
      else
      {
        out <- rep(0, length(x))
        out[...] <- grad
        out
      }
    }
  )
))

#' @export
#' @author Ron Triepels
`[.cg_node` <- function(x, ...)
{
  cg_subset1(x, ...)
}

#' Subset
#'
#' Calculate \code{x[[...]]}.
#'
#' @param x either a cg_node object or a numerical vector or array.
#' @param ... either cg_node objects or a numerical scalars that are passed on to the \code{`[[`} function.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable with respect to the arguments provided to \code{...}. Any attempt to differentiate this operator with respect to these arguments results in an error.
#'
#' @seealso \link[base:Extract]{subset}
#'
#' @author Ron Triepels
#' @export
cg_subset2 <- function(x, ..., name = NULL)
{
  cg_operator(subset2, c(x, dots()))
}

# Function definition
delayedAssign("subset2", cg_function(
  def = base::`[[`,
  grads = list(
    function(x, ..., exact = TRUE, value, grad)
    {
      if(!is.numeric(x))
      {
        stop(sprintf("cannot differentiate object of type '%s'", typeof(x)))
      }

      if(is.array(x))
      {
        out <- array(0, dim(x))
        out[[...]] <- grad
        out
      }
      else
      {
        out <- rep(0, length(x))
        out[[...]] <- grad
        out
      }
    }
  )
))

#' @export
#' @author Ron Triepels
`[[.cg_node` <- function(x, ...)
{
  cg_subset2(x, ...)
}
