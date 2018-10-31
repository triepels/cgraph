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

#' Default Function Library
#'
#' This is the default function library used by a computational graph. It implements all graph operators by base R functions.
#'
#' @seealso \link[base]{base}
#'
#' @export
cg_default_library <- new.env(parent = emptyenv())

#
#
# Math Functions
#
#

# Base functions
cg_default_library$`+` <- `+`
cg_default_library$`-` <- `-`
cg_default_library$`*` <- `*`
cg_default_library$`/` <- `/`
cg_default_library$`^` <- `^`
cg_default_library$sqrt <- sqrt
cg_default_library$exp <- exp
cg_default_library$log <- log
cg_default_library$log2 <- log2
cg_default_library$log10 <- log10
cg_default_library$abs <- abs

#' Positive Gradient
#'
#' Calculate the gradient of \code{x} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pos_grad
#' @keywords internal
cg_default_library$pos_grad <- function(x, val, grad)
{
  grad
}

#' Negative Gradient
#'
#' Calculate the gradient of \code{-x} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{-x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name neg_grad
#' @keywords internal
cg_default_library$neg_grad <- function(x, val, grad)
{
  -grad
}

#' Add Gradient
#'
#' Calculate the gradient of \code{x + y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x + y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name add_grad_x
#' @keywords internal
cg_default_library$add_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad
  }
  else
  {
    bsum(grad, length(x))
  }
}

#' Add Gradient
#'
#' Calculate the gradient of \code{x + y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x + y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name add_grad_y
#' @keywords internal
cg_default_library$add_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad
  }
  else
  {
    bsum(grad, length(y))
  }
}

#' Subtract Gradient
#'
#' Calculate the gradient of \code{x - y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x - y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name sub_grad_x
#' @keywords internal
cg_default_library$sub_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad
  }
  else
  {
    bsum(grad, length(x))
  }
}

#' Subtract Gradient
#'
#' Calculate the gradient of \code{x - y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x - y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name sub_grad_y
#' @keywords internal
cg_default_library$sub_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    -grad
  }
  else
  {
    bsum(-grad, length(y))
  }
}

#' Multiply Gradient
#'
#' Calculate the gradient of \code{x * y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x * y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name mul_grad_x
#' @keywords internal
cg_default_library$mul_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * y
  }
  else
  {
    bsum(grad * y, length(x))
  }
}

#' Multiply Gradient
#'
#' Calculate the gradient of \code{x * y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x * y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name mul_grad_y
#' @keywords internal
cg_default_library$mul_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad * x
  }
  else
  {
    bsum(grad * x, length(y))
  }
}

#' Divide Gradient
#'
#' Calculate the gradient of \code{x / y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x / y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name div_grad_x
#' @keywords internal
cg_default_library$div_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad / y
  }
  else
  {
    bsum(grad / y, length(x))
  }
}

#' Divide Gradient
#'
#' Calculate the gradient of \code{x / y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x / y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name div_grad_y
#' @keywords internal
cg_default_library$div_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    -grad * x / y^2
  }
  else
  {
    bsum(-grad * x / y^2, length(y))
  }
}

#' Power Gradient
#'
#' Calculate the gradient of \code{x^y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x ^ y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pow_grad_x
#' @keywords internal
cg_default_library$pow_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * y * x ^ (y - 1)
  }
  else
  {
    bsum(grad * y * x ^ (y - 1), length(x))
  }
}

#' Power Gradient
#'
#' Calculate the gradient of \code{x^y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x ^ y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pow_grad_y
#' @keywords internal
cg_default_library$pow_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad * x^y * log(x)
  }
  else
  {
    bsum(grad * x^y * log(x), length(y))
  }
}

#' Square Root Gradient
#'
#' Calculate the gradient of \code{sqrt(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{sqrt(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name sqrt_grad
#' @keywords internal
cg_default_library$sqrt_grad <- function(x, val, grad)
{
  grad * 1 / (2 * val)
}

#' Exponential Gradient Function
#'
#' Calculate the gradient of \code{exp(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{exp(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name exp_grad
#' @keywords internal
cg_default_library$exp_grad <- function(x, val, grad)
{
  grad * val
}

#' Natural Logarithm Gradient
#'
#' Calculate the gradient of \code{log(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{log(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name ln_grad
#' @keywords internal
cg_default_library$ln_grad <- function(x, val, grad)
{
  grad / x
}

#' Logarithmic Base 2 Gradient
#'
#' Calculate the gradient of \code{log2(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{log2(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name log2_grad
#' @keywords internal
cg_default_library$log2_grad <- function(x, val, grad)
{
  grad / (x * log(2))
}

#' Logarithmic Base 10 Gradient
#'
#' Calculate the gradient of \code{log10(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{log10(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name log10_grad
#' @keywords internal
cg_default_library$log10_grad <- function(x, val, grad)
{
  grad / (x * log(10))
}

#' Absolute Value Gradient
#'
#' Calculate the gradient of \code{abs(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{abs(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name abs_grad
#' @keywords internal
cg_default_library$abs_grad <- function(x, val, grad)
{
  grad * (x / val)
}

# Logical Functions

# Base functions
cg_default_library$`!` <- `!`
cg_default_library$`==` <- `==`
cg_default_library$`!=` <- `!=`
cg_default_library$`<` <- `<`
cg_default_library$`>` <- `>`
cg_default_library$`<=` <- `<=`
cg_default_library$`>=` <- `>=`

#' Not Gradient
#'
#' Calculate the gradient of \code{!x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{!x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name not_grad
#' @keywords internal
cg_default_library$not_grad <- function(x, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Equal Gradient
#'
#' Calculate the gradient of \code{x == y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x == y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name equal_grad_x
#' @keywords internal
cg_default_library$equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Equal Gradient
#'
#' Calculate the gradient of \code{x == y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x == y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name equal_grad_y
#' @keywords internal
cg_default_library$equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' Not Equal Gradient
#'
#' Calculate the gradient of \code{x != y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x != y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name not_equal_grad_x
#' @keywords internal
cg_default_library$not_equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Not Equal Gradient
#'
#' Calculate the gradient of \code{x != y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x != y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name not_equal_grad_y
#' @keywords internal
cg_default_library$not_equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' Less Gradient
#'
#' Calculate the gradient of \code{x < y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x < y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name less_grad_x
#' @keywords internal
cg_default_library$less_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Less Gradient
#'
#' Calculate the gradient of \code{x < y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x < y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name less_grad_y
#' @keywords internal
cg_default_library$less_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' Greater Gradient
#'
#' Calculate the gradient of \code{x > y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x > y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name greater_grad_x
#' @keywords internal
cg_default_library$greater_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Greater Gradient
#'
#' Calculate the gradient of \code{x > y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x > y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name greater_grad_y
#' @keywords internal
cg_default_library$greater_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' Less or Equal Gradient
#'
#' Calculate the gradient of \code{x <= y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x <= y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name less_equal_grad_x
#' @keywords internal
cg_default_library$less_equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Less or Equal Gradient
#'
#' Calculate the gradient of \code{x <= y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x <= y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name less_equal_grad_y
#' @keywords internal
cg_default_library$less_equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' Greater or Equal Gradient
#'
#' Calculate the gradient of \code{x >= y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x >= y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name greater_equal_grad_x
#' @keywords internal
cg_default_library$greater_equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Greater or Equal Gradient
#'
#' Calculate the gradient of \code{x >= y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x >= y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name greater_equal_grad_y
#' @keywords internal
cg_default_library$greater_equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#
#
# Trigonometric Functions
#
#

# Base functions
cg_default_library$sin <- sin
cg_default_library$cos <- cos
cg_default_library$tan <- tan
cg_default_library$sinh <- sinh
cg_default_library$cosh <- cosh
cg_default_library$tanh <- tanh
cg_default_library$asin <- asin
cg_default_library$acos <- acos
cg_default_library$atan <- atan
cg_default_library$asinh <- asinh
cg_default_library$acosh <- acosh
cg_default_library$atanh <- atanh

#' Sine Gradient
#'
#' Calculate the gradient of \code{sin(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{sin(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name sin_grad
#' @keywords internal
cg_default_library$sin_grad <- function(x, val, grad)
{
  grad * cos(x)
}

#' Cosine Gradient
#'
#' Calculate the gradient of \code{cos(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{cos(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name cos_grad
#' @keywords internal
cg_default_library$cos_grad <- function(x, val, grad)
{
  -grad * sin(x)
}

#' Tangent Gradient
#'
#' Calculate the gradient of \code{tan(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{tan(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name tan_grad
#' @keywords internal
cg_default_library$tan_grad <- function(x, val, grad)
{
  grad / cos(x)^2
}

#' Hyperbolic Sine Gradient
#'
#' Calculate the gradient of \code{sinh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{sinh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name sinh_grad
#' @keywords internal
cg_default_library$sinh_grad <- function(x, val, grad)
{
  grad * cosh(x)
}

#' Hyperbolic Cosine Gradient
#'
#' Calculate the gradient of \code{cosh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{cosh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name cosh_grad
#' @keywords internal
cg_default_library$cosh_grad <- function(x, val, grad)
{
  grad * sinh(x)
}

#' Hyperbolic Tangent Gradient
#'
#' Calculate the gradient of \code{tanh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{tanh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name tanh_grad
#' @keywords internal
cg_default_library$tanh_grad <- function(x, val, grad)
{
  grad * (1 - val^2)
}

#' Inverse Sine Gradient
#'
#' Calculate the gradient of \code{asin(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{asin(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name asin_grad
#' @keywords internal
cg_default_library$asin_grad <- function(x, val, grad)
{
  grad / sqrt(1 - x^2)
}

#' Inverse Cosine Gradient
#'
#' Calculate the gradient of \code{acos(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{acos(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name acos_grad
#' @keywords internal
cg_default_library$acos_grad <- function(x, val, grad)
{
  -grad / sqrt(1 - x^2)
}

#' Inverse Tangent Gradient
#'
#' Calculate the gradient of \code{atan(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{atan(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name atan_grad
#' @keywords internal
cg_default_library$atan_grad <- function(x, val, grad)
{
  grad / (x^2 + 1)
}

#' Inverse Hyperbolic Sine Gradient
#'
#' Calculate the gradient of \code{asinh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{asinh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name asinh_grad
#' @keywords internal
cg_default_library$asinh_grad <- function(x, val, grad)
{
  grad / sqrt(x^2 + 1)
}

#' Inverse Hyperbolic Cosine Gradient
#'
#' Calculate the gradient of \code{acosh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{acosh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name acosh_grad
#' @keywords internal
cg_default_library$acosh_grad <- function(x, val, grad)
{
  grad / sqrt(x^2 - 1)
}

#' Inverse Hyperbolic Tangent Gradient
#'
#' Calculate the gradient of \code{atanh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{atanh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name atanh_grad
#' @keywords internal
cg_default_library$atanh_grad <- function(x, val, grad)
{
  grad / (1 - x^2)
}

#
#
# Array Functions
#
#

# Base functions
cg_default_library$`%*%` <- `%*%`
cg_default_library$crossprod <- crossprod
cg_default_library$tcrossprod <- tcrossprod
cg_default_library$sum <- sum
cg_default_library$prod <- prod
cg_default_library$rowSums <- rowSums
cg_default_library$colSums <- colSums
cg_default_library$mean <- mean
cg_default_library$rowMeans <- rowMeans
cg_default_library$colMeans <- colMeans
cg_default_library$max <- max
cg_default_library$min <- min
cg_default_library$pmax <- pmax
cg_default_library$pmin <- pmin

#' Matrix Multiplication Gradient
#'
#' Calculate the gradient of \code{x \%*\% y} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{x \%*\% y}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name matmul_grad_x
#' @keywords internal
cg_default_library$matmul_grad_x <- function(x, y, val, grad)
{
  tcrossprod(grad, y)
}

#' Matrix Multiplication Gradient
#'
#' Calculate the gradient of \code{x \%*\% y} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{x \%*\% y}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name matmul_grad_y
#' @keywords internal
cg_default_library$matmul_grad_y <- function(x, y, val, grad)
{
  crossprod(x, grad)
}

#' Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{crossprod(x, y)} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{crossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name crossprod_grad_x
#' @keywords internal
cg_default_library$crossprod_grad_x <- function(x, y, val, grad)
{
  y %*% grad
}

#' Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{crossprod(x, y)} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{crossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name crossprod_grad_y
#' @keywords internal
cg_default_library$crossprod_grad_y <- function(x, y, val, grad)
{
  x %*% grad
}

#' Transpose Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{tcrossprod(x, y)} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{tcrossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name tcrossprod_grad_x
#' @keywords internal
cg_default_library$tcrossprod_grad_x <- function(x, y, val, grad)
{
  grad %*% y
}

#' Transpose Matrix Crossproduct Gradient
#'
#' Calculate the gradient of \code{tcrossprod(x, y)} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{tcrossprod(x, y)}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name tcrossprod_grad_y
#' @keywords internal
cg_default_library$tcrossprod_grad_y <- function(x, y, val, grad)
{
  grad %*% x
}

#' Linear Transformation
#'
#' Calculate the linear transformation \code{x \%*\% y + c(z)}.
#'
#' @param x numeric matrix.
#' @param y numeric matrix.
#' @param z numeric vector.
#'
#' @return numeric matrix, result of the transformation.
#'
#' @author Ron Triepels
#'
#' @name linear
#' @keywords internal
cg_default_library$linear <- function(x, y, z)
{
  x %*% y + c(z)
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{linear(x, y, z)} with respect to \code{x}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{linear(x, y, z)}.
#' @param grad numeric matrix, gradient of \code{x}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name linear_grad_x
#' @keywords internal
cg_default_library$linear_grad_x  <- function(x, y, z, val, grad)
{
  tcrossprod(grad, y)
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{linear(x, y, z)} with respect to \code{y}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{linear(x, y, z)}.
#' @param grad numeric matrix, gradient of \code{y}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name linear_grad_y
#' @keywords internal
cg_default_library$linear_grad_y <- function(x, y, z, val, grad)
{
  crossprod(x, grad)
}

#' Linear Transformation Gradient
#'
#' Calculate the gradient of \code{linear(x, y, z)} with respect to \code{z}.
#'
#' @param x numeric matrix, value of \code{x}.
#' @param y numeric matrix, value of \code{y}.
#' @param val numeric matrix, value of \code{linear(x, y, z)}.
#' @param grad numeric matrix, gradient of \code{z}.
#'
#' @return numeric matrix, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name linear_grad_z
#' @keywords internal
cg_default_library$linear_grad_z <- function(x, y, z, val, grad)
{
  if(is.array(z))
  {
    array(rowSums(grad), dim(z))
  }
  else
  {
    bsum(grad, length(z))
  }
}

#' Sum of Vector Elements Gradient
#'
#' Calculate the gradient of \code{sum(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{sum(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name sum_grad
#' @keywords internal
cg_default_library$sum_grad <- function(x, val, grad)
{
  if(is.array(x))
  {
    array(grad, dim(x))
  }
  else
  {
    rep(grad, length(x))
  }
}

#' Product of Vector Elements Gradient
#'
#' Calculate the gradient of \code{prod(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{prod(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name prod_grad
#' @keywords internal
cg_default_library$prod_grad <- function(x, val, grad)
{
  grad * val / x
}

#' Row Sums Gradient
#'
#' Calculate the gradient of \code{rowSums(x)} with respect to \code{x}.
#'
#' @param x numeric array, value of \code{x}.
#' @param val numeric array, value of \code{rowSums(x)}.
#' @param grad numeric array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name rowSums_grad
#' @keywords internal
cg_default_library$rowSums_grad <- function(x, val, grad)
{
  array(grad, dim(x))
}

#' Column Sums Gradient
#'
#' Calculate the gradient of \code{colSums(x)} with respect to \code{x}.
#'
#' @param x numeric array, value of \code{x}.
#' @param val numeric array, value of \code{colSums(x)}.
#' @param grad numeric array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name colSums_grad
#' @keywords internal
cg_default_library$colSums_grad <- function(x, val, grad)
{
  aperm(array(grad, rev(dim(x))))
}

#' Arithmetic Mean Gradient
#'
#' Calculate the gradient of \code{mean(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{mean(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name mean_grad
#' @keywords internal
cg_default_library$mean_grad <- function(x, val, grad)
{
  if(is.array(x))
  {
    1 / length(x) * array(grad, dim(x))
  }
  else
  {
    1 / length(x) * rep(grad, length(x))
  }
}

#' Row Means Gradient
#'
#' Calculate the gradient of \code{rowMeans(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{rowMeans(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name rowMeans_grad
#' @keywords internal
cg_default_library$rowMeans_grad <- function(x, val, grad)
{
  1 / prod(dim(x)[-1L]) * array(grad, dim(x))
}

#' Column Means Gradient
#'
#' Calculate the gradient of \code{colMeans(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{colMeans(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name colMeans_grad
#' @keywords internal
cg_default_library$colMeans_grad <- function(x, val, grad)
{
  1 / dim(x)[1] * aperm(array(grad, rev(dim(x))))
}

#' Maxima Gradient
#'
#' Calculate the gradient of \code{max(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{max(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name max_grad
#' @keywords internal
cg_default_library$max_grad <- function(x, val, grad)
{
  c(grad) * (x == c(val))
}

#' Minima Gradient
#'
#' Calculate the gradient of \code{min(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{min(y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name min_grad
#' @keywords internal
cg_default_library$min_grad <- function(x, val, grad)
{
  c(grad) * (x == c(val))
}

#' Parallel Maxima Gradient
#'
#' Calculate the gradient of \code{pmax(x, y)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{pmax(x, y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pmax_grad_x
#' @keywords internal
cg_default_library$pmax_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * (x >= c(y))
  }
  else
  {
    bsum(grad * (x >= c(y)), length(x))
  }
}

#' Parallel Maxima Gradient
#'
#' Calculate the gradient of \code{pmax(x, y)} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{pmax(x, y)}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pmax_grad_y
#' @keywords internal
cg_default_library$pmax_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad * (x < c(y))
  }
  else
  {
    bsum(grad * (x < c(y)), length(y))
  }
}

#' Parallel Minima Gradient
#'
#' Calculate the gradient of \code{pmin(x, y)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{pmin(x, y)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pmin_grad_x
#' @keywords internal
cg_default_library$pmin_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * (x <= c(y))
  }
  else
  {
    bsum(grad * (x <= c(y)), length(x))
  }
}

#' Parallel Minima Gradient
#'
#' Calculate the gradient of \code{pmin(x, y)} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{pmin(x, y)}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name pmin_grad_y
#' @keywords internal
cg_default_library$pmin_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    grad * (x > c(y))
  }
  else
  {
    bsum(grad * (x > c(y)), length(y))
  }
}

#
#
# Transformation Functions
#
#

# Base functions
cg_default_library$as.double <- as.double
cg_default_library$array <- array
cg_default_library$t <- t

#' Coerce to a Numeric Vector Gradient
#'
#' Calculate the gradient of \code{as.numeric(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{as.numeric(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name as_double_grad
#' @keywords internal
cg_default_library$as_double_grad <- function(x, val, grad)
{
  if(is.array(x))
  {
    array(grad, dim(x))
  }
  else
  {
    as.double(grad)
  }
}

#' Reshape Array Dimensions Gradient
#'
#' Calculate the gradient of \code{array(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param dim numeric scalar or vector, value of \code{dim}.
#' @param val numeric vector or array, value of \code{array(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name reshape_grad_x
#' @keywords internal
cg_default_library$reshape_grad_x <- function(x, dim, val, grad)
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

#' Reshape Array Dimensions Gradient
#'
#' Calculate the gradient of \code{array(x)} with respect to \code{dim}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param dim numeric scalar or vector, value of \code{dim}.
#' @param val numeric vector or array, value of \code{array(x)}.
#' @param grad numeric vector or array, gradient of \code{dim}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name reshape_grad_dim
#' @keywords internal
cg_default_library$reshape_grad_dim <- function(x, dim, val, grad)
{
  stop("unable to differentiate argument 'dim' of operator 'cg_reshape'")
}

#' Matrix Transpose Gradient
#'
#' Calculate the gradient of \code{t(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{t(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#'
#' @name t_grad
#' @keywords internal
cg_default_library$t_grad <- function(x, val, grad)
{
  t(grad)
}

#
#
# Miscellaneous Functions
#
#

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
#'
#' @name sigmoid
#' @keywords internal
cg_default_library$sigmoid <- function(x)
{
  .Call("sigmoid", x, PACKAGE = "cgraph")
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
#'
#' @name sigmoid_grad
#' @keywords internal
cg_default_library$sigmoid_grad <- function(x, val, grad)
{
  grad * val * (1 - val)
}
