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
#' @param x a cg_node object.
#' @param ... either cg_node objects or numerical scalars that are passed on to the \code{`[`} function.
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
  cg_operator(.subset1, c(x = x, dots()), name)
}

# Function definition
delayedAssign(".subset1", cg_function(
  def = base::`[`,
  grads = list(
    x = function(x, ..., drop = TRUE, value, grad)
    {
      x[] <- 0
      x[...] <- grad
      x
    }
  )
))

#' @export
#' @author Ron Triepels
`[.cg_node` <- function(x, ...)
{
  cg_subset1(x, ...)
}

#' Subassign
#'
#' Calculate \code{x[...] <- y}.
#'
#' @param x a cg_node object.
#' @param ... either cg_node objects or numerical scalars that are passed on to the \code{`[<-`} function.
#' @param y either a cg_node or an array of a similar class as \code{x}.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable with respect to the arguments provided to \code{...}. Any attempt to differentiate this operator with respect to these arguments results in an error.
#'
#' @seealso \link[base:Extract]{subassign}
#'
#' @author Ron Triepels
#' @export
cg_subassign1 <- function(x, ..., y, name = NULL)
{
  cg_operator(.subassign1, c(x = x, dots(), y = y), name)
}

# Function definition
delayedAssign(".subassign1", cg_function(
  def = base::`[<-`,
  grads = list(
    x = function(x, ..., y, value, grad)
    {
      grad[...] <- 0
      grad
    },
    y = function(x, ..., y, value, grad)
    {
      grad[...]
    }
  )
))

#' @export
#' @author Ron Triepels
`[<-.cg_node` <- function(x, ..., value)
{
  cg_subassign1(x, ..., y = value)
}

#' Subset
#'
#' Calculate \code{x[[...]]}.
#'
#' @param x a cg_node object.
#' @param ... either cg_node objects or numerical scalars that are passed on to the \code{`[[`} function.
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
  cg_operator(.subset2, c(x = x, dots()), name)
}

# Function definition
delayedAssign(".subset2", cg_function(
  def = base::`[[`,
  grads = list(
    x = function(x, ..., exact = TRUE, value, grad)
    {
      x[] <- 0
      x[[...]] <- grad
      x
    }
  )
))

#' @export
#' @author Ron Triepels
`[[.cg_node` <- function(x, ...)
{
  cg_subset2(x, ...)
}

#' Subassign
#'
#' Calculate \code{x[[i]] <- y}.
#'
#' @param x a cg_node object.
#' @param i either a cg_node or a numerical scalar that is passed on to the \code{`[[<-`} function.
#' @param y either a cg_node or an array of a similar class as \code{x}.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg_operator object.
#'
#' @note This operator is not differentiable with respect to \code{i}. Any attempt to differentiate this operator with respect to this argument results in an error.
#'
#' @seealso \link[base:Extract]{subassign}
#'
#' @author Ron Triepels
#' @export
cg_subassign2 <- function(x, i, y, name = NULL)
{
  cg_operator(.subassign2, list(x = x, i = i, y = y), name)
}

# Function definition
delayedAssign(".subassign2", cg_function(
  def = base::`[[<-`,
  grads = list(
    x = function(x, i, y, value, grad)
    {
      grad[[i]] <- 0
      grad
    },
    y = function(x, i, y, value, grad)
    {
      grad[[...]]
    }
  )
))

#' @export
#' @author Ron Triepels
`[[<-.cg_node` <- function(x, i, value)
{
  cg_subassign1(x, i, y = value)
}
