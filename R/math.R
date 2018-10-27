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

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{positive}
#'
#' @author Ron Triepels
cg_pos <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`+`),
    grads = list(
      quote(pos_grad)
    ),
    args = list(x)
  )
}

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
#' @keywords internal
pos_grad <- function(x, val, grad)
{
  grad
}

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{negative}
#'
#' @author Ron Triepels
cg_neg <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`-`),
    grads = list(
      quote(neg.grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
neg.grad <- function(x, val, grad)
{
  -grad
}

#' Add
#'
#' Calculate \code{x + y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{add}
#'
#' @author Ron Triepels
cg_add <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = as.name("+"),
    grads = list(
      as.name("add_grad_x"),
      as.name("add_grad_y")
    ),
    args = list(x, y)
  )
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
#' @keywords internal
add_grad_x <- function(x, y, val, grad)
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
#' @keywords internal
add_grad_y <- function(x, y, val, grad)
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

# S3 method
`+.cg.node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg_pos(x)
  }
  else
  {
    cgraph::cg_add(x, y)
  }
}

#' Subtract
#'
#' Calculate \code{x - y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{subtract}
#'
#' @author Ron Triepels
cg_sub <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`-`),
    grads = list(
      quote(sub_grad_x),
      quote(sub_grad_y)
    ),
    args = list(x, y)
  )
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
#' @keywords internal
sub_grad_x <- function(x, y, val, grad)
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
#' @keywords internal
sub_grad_y <- function(x, y, val, grad)
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

# S3 method
`-.cg.node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg_neg(x)
  }
  else
  {
    cgraph::cg_sub(x, y)
  }
}

#' Multiply
#'
#' Calculate \code{x * y} element-wise.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{multiply}
#'
#' @author Ron Triepels
cg_mul <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`*`),
    grads = list(
      x = quote(mul_grad_x),
      y = quote(mul_grad_y)
    ),
    args = list(x, y)
  )
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
#' @keywords internal
mul_grad_x <- function(x, y, val, grad)
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
#' @keywords internal
mul_grad_y <- function(x, y, val, grad)
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

# S3 method
`*.cg.node` <- function(x, y)
{
  cgraph::cg_mul(x, y)
}

#' Divide
#'
#' Calculate \code{x / y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{divide}
#'
#' @author Ron Triepels
cg_div <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`/`),
    grads = list(
      quote(div_grad_x),
      quote(div_grad_y)
    ),
    args = list(x, y)
  )
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
#' @keywords internal
div_grad_x <- function(x, y, val, grad)
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
#' @keywords internal
div_grad_y <- function(x, y, val, grad)
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

# S3 method
`/.cg.node` <- function(x, y)
{
  cgraph::cg_div(x, y)
}

#' Power
#'
#' Calculate \code{x ^ y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Arithmetic]{power}
#'
#' @author Ron Triepels
cg_pow <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`^`),
    grads = list(
      quote(pow_grad_x),
      quote(pow_grad_y)
    ),
    args = list(x, y)
  )
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
#' @keywords internal
pow_grad_x <- function(x, y, val, grad)
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
#' @keywords internal
pow_grad_y <- function(x, y, val, grad)
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

# S3 method
`^.cg.node` <- function(x, y)
{
  cgraph::cg_pow(x, y)
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:MathFun]{sqrt}
#'
#' @author Ron Triepels
cg_sqrt <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sqrt),
    grads = list(
      quote(sqrt_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
sqrt_grad <- function(x, val, grad)
{
  grad * 1 / (2 * val)
}

#' Exponential Function
#'
#' Calculate \code{exp(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{exp}
#'
#' @author Ron Triepels
cg_exp <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(exp),
    grads = list(
      quote(exp_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
exp_grad <- function(x, val, grad)
{
  grad * val
}

#' Natural Logarithm
#'
#' Calculate \code{log(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{log}
#'
#' @author Ron Triepels
cg_ln <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(log),
    grads = list(
      quote(ln_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
ln_grad <- function(x, val, grad)
{
  grad / x
}

#' Logarithm Base 2
#'
#' Calculate \code{log2(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{log}
#'
#' @author Ron Triepels
cg_log2 <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(log2),
    grads = list(
      quote(log2_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
log2_grad <- function(x, val, grad)
{
  grad / (x * log(2))
}

#' Logarithmic Base 10
#'
#' Calculate \code{log10(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:log]{log10}
#'
#' @author Ron Triepels
cg_log10 <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(log10),
    grads = list(
      quote(log10_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
log10_grad <- function(x, val, grad)
{
  grad / (x * log(10))
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:MathFun]{abs}
#'
#' @author Ron Triepels
cg_abs <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(abs),
    grads = list(
      quote(abs_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
abs_grad <- function(x, val, grad)
{
  grad * (x / val)
}
