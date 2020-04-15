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
  cg_operator(fun_subset1, c(x = x, dots()))
}

# Function definition
delayedAssign("fun_subset1", cg_function(
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
  cg_operator(fun_subassign1, c(x = x, dots(), y = y))
}

# Function definition
delayedAssign("fun_subassign1", cg_function(
  def = base::`[<-`,
  grads = list(
    x = function(x, ..., y, value, grad)
    {
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
  cg_operator(fun_subset2, c(x = x, dots()))
}

# Function definition
delayedAssign("fun_subset2", cg_function(
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
  cg_operator(fun_subassign2, list(x = x, i = i, y = y))
}

# Function definition
delayedAssign("fun_subassign2", cg_function(
  def = base::`[[<-`,
  grads = list(
    x = function(x, i, y, value, grad)
    {
      grad
    },
    y = function(x, i, y, value, grad)
    {
      grad[[i]]
    }
  )
))

#' @export
#' @author Ron Triepels
`[[<-.cg_node` <- function(x, i, value)
{
  cg_subassign2(x, i, y = value)
}

#' @author Ron Triepels
#' @export
cg_slice <- function(x, index, name = NULL)
{
  cg_operator(fun_slice, list(x = x, index = index), name)
}

# Function definition
delayedAssign("fun_slice", cg_function(
  def = function(x, index)
  {
    .Call("slice", x, index, PACKAGE = "cgraph")
  },
  grads = list(
    x = function(x, index, value, grad)
    {
      x[] <- 0
      .Call("slice_assign", x, index, grad, PACKAGE = "cgraph")
    }
  )
))

#' @author Ron Triepels
#' @export
cg_slice_assign <- function(x, index, y, name = NULL)
{
  cg_operator(fun_slice_assign, list(x = x, index = index, y = y), name)
}

# Function definition
delayedAssign("fun_slice_assign", cg_function(
  def = function(x, index, y)
  {
    .Call("slice_assign", x, index, y, PACKAGE = "cgraph")
  },
  grads = list(
    x = function(x, index, y, value, grad)
    {
      grad
    },
    y = function(x, index, y, value, grad)
    {
      .Call("slice", grad, index, PACKAGE = "cgraph")
    }
  )
))

#' @author Ron Triepels
#' @export
`cg_slice<-` <- function(x, index, value)
{
  cg_slice_assign(x, index, y = value)
}
