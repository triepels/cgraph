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
cg.add <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x + y),
    grads = list(
      x = quote(cg.add.grad(x, grad)),
      y = quote(cg.add.grad(y, grad))
    ),
    binding = list(x = x, y = y)
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
cg.add.grad <- function(x, grad)
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
cg.pos <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x),
    grads = list(x = quote(grad)),
    binding = list(x = x)
  )
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
cg.sub <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x - y),
    grads = list(
      x = quote(cg.add.grad(x, grad)),
      y = quote(cg.sub.grad(y, grad))
    ),
    binding = list(x = x, y = y)
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
cg.sub.grad <- function(y, grad)
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
cg.neg <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(-x),
    grads = list(x = quote(-grad)),
    binding = list(x = x)
  )
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
cg.mul <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x * y),
    grads = list(
      x = quote(cg.mul.grad.x(x, y, grad)),
      y = quote(cg.mul.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
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
cg.mul.grad.x <- function(x, y, grad)
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
cg.mul.grad.y <- function(x, y, grad)
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
cg.div <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x / y),
    grads = list(
      x = quote(cg.div.grad.x(x, y, grad)),
      y = quote(cg.div.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
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
cg.div.grad.x <- function(x, y, grad)
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
cg.div.grad.y <- function(x, y, grad)
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
cg.pow <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x^y),
    grads = list(
      x = quote(cg.pow.grad.x(x, y, grad)),
      y = quote(cg.pow.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
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
cg.pow.grad.x <- function(x, y, grad)
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
cg.pow.grad.y <- function(x, y, grad)
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
cg.sqrt <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(sqrt(x)),
    grads = list(x = quote(cg.sqrt.grad(x, grad))),
    binding = list(x = x)
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
cg.sqrt.grad <- function(x, grad)
{
  grad * 1 / (2 * sqrt(x))
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
sqrt.cg.node <- function(x)
{
  .Deprecated("cg.sqrt")

  cgraph::cg.sqrt(x)
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
cg.exp <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(exp(x)),
    grads = list(x = quote(cg.exp.grad(x, grad))),
    binding = list(x = x)
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
cg.exp.grad <- function(x, grad)
{
  grad * exp(x)
}

#' Exponential Function
#'
#' Calculate \code{exp(1)^x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
exp.cg.node <- function(x)
{
  .Deprecated("cg.exp")

  cgraph::cg.exp(x)
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
cg.ln <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(log(x)),
    grads = list(x = quote(cg.ln.grad(x, grad))),
    binding = list(x = x)
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
cg.ln.grad <- function(x, grad)
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
cg.log2 <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(log2(x)),
    grads = list(x = quote(cg.log2.grad(x, grad))),
    binding = list(x = x)
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
cg.log2.grad <- function(x, grad)
{
  grad / (x * log(2))
}

#' Logarithmic Base 2 Function
#'
#' Calculate \code{log2(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
log2.cg.node <- function(x)
{
  .Deprecated("cg.log2")

  cgraph::cg.log2(x)
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
cg.log10 <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(log10(x)),
    grads = list(x = quote(cg.log10.grad(x, grad))),
    binding = list(x = x)
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
cg.log10.grad <- function(x, grad)
{
  grad / (x * log(10))
}

#' Logarithmic Base 10 Function
#'
#' Calculate \code{log10(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
log10.cg.node <- function(x)
{
  .Deprecated("cg.log10")

  cgraph::cg.log10(x)
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
cg.abs <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(abs(x)),
    grads = list(x = quote(cg.abs.grad(x, grad))),
    binding = list(x = x)
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
cg.abs.grad <- function(x, grad)
{
  grad * (x / abs(x))
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
abs.cg.node <- function(x)
{
  .Deprecated("cg.abs")

  cgraph::cg.abs(x)
}
