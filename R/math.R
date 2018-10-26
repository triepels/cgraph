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
#' @author Ron Triepels
cg.add <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = as.name("+"),
    grads = list(
      as.name("add.grad.x"),
      as.name("add.grad.y")
    ),
    args = list(x, y)
  )
}

#' Add Gradient
#'
#' Calculate the gradient of \code{x + y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
add.grad.x <- function(x, y, val, grad)
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

add.grad.y <- function(x, y, val, grad)
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

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pos <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(`+`),
    grads = list(
      quote(pos.grad)
    ),
    args = list(x)
  )
}

pos.grad <- function(x, val, grad)
{
  grad
}

# S3 method
`+.cg.node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg.pos(x)
  }
  else
  {
    cgraph::cg.add(x, y)
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
#' @author Ron Triepels
cg.sub <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`-`),
    grads = list(
      quote(sub.grad.x),
      quote(sub.grad.y)
    ),
    args = list(x, y)
  )
}

#' Subtract Gradient
#'
#' Calculate the gradient of \code{x - y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
sub.grad.x <- function(x, y, val, grad)
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

sub.grad.y <- function(x, y, val, grad)
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

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.neg <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(`-`),
    grads = list(
      quote(neg.grad)
    ),
    args = list(x)
  )
}

neg.grad <- function(x, val, grad)
{
  -grad
}

# S3 method
`-.cg.node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg.neg(x)
  }
  else
  {
    cgraph::cg.sub(x, y)
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
#' @author Ron Triepels
cg.mul <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`*`),
    grads = list(
      x = quote(mul.grad.x),
      y = quote(mul.grad.y)
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
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
mul.grad.x <- function(x, y, val, grad)
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
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
mul.grad.y <- function(x, y, val, grad)
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
  cgraph::cg.mul(x, y)
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
#' @author Ron Triepels
cg.div <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`/`),
    grads = list(
      quote(div.grad.x),
      quote(div.grad.y)
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
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
div.grad.x <- function(x, y, val, grad)
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
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
div.grad.y <- function(x, y, val, grad)
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
  cgraph::cg.div(x, y)
}

#' Power
#'
#' Calculate \code{x^y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pow <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`^`),
    grads = list(
      quote(pow.grad.x),
      quote(pow.grad.y)
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
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
pow.grad.x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    grad * y * x^(y - 1)
  }
  else
  {
    bsum(grad * y * x^(y - 1), length(x))
  }
}

#' Power Gradient
#'
#' Calculate the gradient of \code{x^y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
pow.grad.y <- function(x, y, val, grad)
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
  cgraph::cg.pow(x, y)
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
#' @author Ron Triepels
cg.sqrt <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(sqrt),
    grads = list(
      quote(sqrt.grad)
    ),
    args = list(x)
  )
}

#' Square Root Gradient
#'
#' Calculate the gradient of \code{sqrt(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
sqrt.grad <- function(x, val, grad)
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
#' @author Ron Triepels
cg.exp <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(exp),
    grads = list(
      quote(exp.grad)
    ),
    args = list(x)
  )
}

#' Exponential Gradient Function
#'
#' Calculate the gradient of \code{exp(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
exp.grad <- function(x, val, grad)
{
  grad * val
}

#' Natural Logarithmic Function
#'
#' Calculate \code{log(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.ln <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(log),
    grads = list(
      quote(ln.grad)
    ),
    args = list(x)
  )
}

#' Natural Logarithmic Gradient Gradient
#'
#' Calculate the gradient of \code{log(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
ln.grad <- function(x, val, grad)
{
  grad / x
}

#' Logarithmic Base 2 Function
#'
#' Calculate \code{log2(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.log2 <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(log2),
    grads = list(
      quote(log2.grad)
    ),
    args = list(x)
  )
}

#' Logarithmic Base 2 Gradient Function
#'
#' Calculate the gradient of \code{log2(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
log2.grad <- function(x, val, grad)
{
  grad / (x * log(2))
}

#' Logarithmic Base 10 Function
#'
#' Calculate \code{log10(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.log10 <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(log10),
    grads = list(
      quote(log10.grad)
    ),
    args = list(x)
  )
}

#' Logarithmic Base 10 Gradient Function
#'
#' Calculate the gradient of \code{log10(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
log10.grad <- function(x, val, grad)
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
#' @author Ron Triepels
cg.abs <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(abs),
    grads = list(
      quote(abs.grad)
    ),
    args = list(x)
  )
}

#' Absolute Value Gradient
#'
#' Calculate the gradient of \code{abs(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
abs.grad <- function(x, val, grad)
{
  grad * (x / val)
}
