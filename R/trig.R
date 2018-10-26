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

#' Sine
#'
#' Calculate \code{sin(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sin <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(sin),
    grads = list(
      x = quote(sin.grad)
    ),
    args = list(x)
  )
}

#' Sine Gradient
#'
#' Calculate the gradient of \code{sin(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
sin.grad <- function(x, val, grad)
{
  grad * cos(x)
}

#' Sine
#'
#' Calculate \code{sin(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
sin.cg.node <- function(x)
{
  .Deprecated("cg.sin")

  cgraph::cg.sin(x)
}

#' Cosine
#'
#' Calculate \code{cos(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.cos <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(cos),
    grads = list(
      quote(cos.grad)
    ),
    args = list(x)
  )
}

#' Cosine Gradient
#'
#' Calculate the gradient of \code{cos(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cos.grad <- function(x, val, grad)
{
  -grad * sin(x)
}

#' Cosine
#'
#' Calculate \code{cos(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cos.cg.node <- function(x)
{
  .Deprecated("cg.cos")

  cgraph::cg.cos(x)
}

#' Tangent
#'
#' Calculate \code{tan(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.tan <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(tan),
    grads = list(
      quote(tan.grad)
    ),
    args = list(x)
  )
}

#' Tangent Gradient
#'
#' Calculate the gradient of \code{tan(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
tan.grad <- function(x, val, grad)
{
  grad / cos(x)^2
}

#' Tangent
#'
#' Calculate \code{tan(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
tan.cg.node <- function(x)
{
  .Deprecated("cg.tan")

  cgraph::cg.tan(x)
}

#' Hyperbolic Sine
#'
#' Calculate \code{sinh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sinh <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(sinh),
    grads = list(
      quote(sinh.grad)
    ),
    args = list(x)
  )
}

#' Hyperbolic Sine Gradient
#'
#' Calculate the gradient of \code{sinh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
sinh.grad <- function(x, val, grad)
{
  grad * cosh(x)
}

#' Hyperbolic Sine
#'
#' Calculate \code{sinh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
sinh.cg.node <- function(x)
{
  .Deprecated("cg.sinh")

  cgraph::cg.sinh(x)
}

#' Hyperbolic Cosine
#'
#' Calculate \code{cosh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.cosh <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(cosh),
    grads = list(
      quote(cosh.grad)
    ),
    args = list(x)
  )
}

#' Hyperbolic Cosine Gradient
#'
#' Calculate the gradient of \code{cosh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cosh.grad <- function(x, val, grad)
{
  grad * sinh(x)
}

#' Hyperbolic Cosine
#'
#' Calculate \code{cosh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cosh.cg.node <- function(x)
{
  .Deprecated("cg.cosh")

  cgraph::cg.cosh(x)
}

#' Hyperbolic Tangent
#'
#' Calculate \code{tanh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.tanh <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(tanh),
    grads = list(
      quote(tanh.grad)
    ),
    args = list(x)
  )
}

#' Hyperbolic Tangent Gradient
#'
#' Calculate the gradient of \code{tanh(x)} with respect to \code{x}.
#'
#' @param y numeric vector or array, value of \code{tanh(x)}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
tanh.grad <- function(x, val, grad)
{
  grad * (1 - val^2)
}

#' Hyperbolic Tangent
#'
#' Calculate \code{tanh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
tanh.cg.node <- function(x)
{
  .Deprecated("cg.tanh")

  cgraph::cg.tanh(x)
}

#' Inverse Sine
#'
#' Calculate \code{asin(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.asin <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(asin),
    grads = list(
      quote(asin.grad)
    ),
    args = list(x)
  )
}

#' Inverse Sine Gradient
#'
#' Calculate the gradient of \code{asin(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
asin.grad <- function(x, val, grad)
{
  grad / sqrt(1 - x^2)
}

#' Inverse Sine
#'
#' Calculate \code{asin(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
asin.cg.node <- function(x)
{
  .Deprecated("cg.asin")

  cgraph::cg.asin(x)
}

#' Inverse Cosine
#'
#' Calculate \code{acos(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.acos <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(acos),
    grads = list(
      quote(acos.grad)
    ),
    args = list(x)
  )
}

#' Inverse Cosine Gradient
#'
#' Calculate the gradient of \code{acos(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
acos.grad <- function(x, val, grad)
{
  -grad / sqrt(1 - x^2)
}

#' Inverse Cosine
#'
#' Calculate \code{acos(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
acos.cg.node <- function(x)
{
  .Deprecated("cg.acos")

  cgraph::cg.acos(x)
}

#' Inverse Tangent
#'
#' Calculate \code{atan(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.atan <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(atan),
    grads = list(
      quote(atan.grad)
    ),
    args = list(x)
  )
}

#' Inverse Tangent Gradient
#'
#' Calculate the gradient of \code{atan(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
atan.grad <- function(x, val, grad)
{
  grad / (x^2 + 1)
}

#' Inverse Tangent
#'
#' Calculate \code{atan(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
atan.cg.node <- function(x)
{
  .Deprecated("cg.atan")

  cgraph::cg.atan(x)
}

#' Inverse Hyperbolic Sine
#'
#' Calculate \code{asinh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.asinh <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(asinh),
    grads = list(
      quote(asinh.grad)
    ),
    args = list(x)
  )
}

#' Inverse Hyperbolic Sine Gradient
#'
#' Calculate the gradient of \code{asinh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
asinh.grad <- function(x, val, grad)
{
  grad / sqrt(x^2 + 1)
}

#' Inverse Hyperbolic Sine
#'
#' Calculate \code{asinh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
asinh.cg.node <- function(x)
{
  .Deprecated("cg.asinh")

  cgraph::cg.asinh(x)
}

#' Inverse Hyperbolic Cosine
#'
#' Calculate \code{acosh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.acosh <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(acosh),
    grads = list(
      quote(acosh.grad)
    ),
    args = list(x)
  )
}

#' Inverse Hyperbolic Cosine Gradient
#'
#' Calculate the gradient of \code{acosh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
acosh.grad <- function(x, val, grad)
{
  grad / sqrt(x^2 - 1)
}

#' Inverse Hyperbolic Cosinus
#'
#' Calculate \code{acosh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
acosh.cg.node <- function(x)
{
  .Deprecated("cg.acosh")

  cgraph::cg.acosh(x)
}

#' Inverse Hyperbolic Tangent
#'
#' Calculate \code{atanh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.atanh <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(atanh),
    grads = list(
      quote(atanh.grad)
    ),
    args = list(x)
  )
}

#' Inverse Hyperbolic Tangent Gradient
#'
#' Calculate the gradient of \code{atanh(x)} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
atanh.grad <- function(x, val, grad)
{
  grad / (1 - x^2)
}

#' Inverse Hyperbolic Tangent
#'
#' Calculate \code{atanh(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
atanh.cg.node <- function(x)
{
  .Deprecated("cg.atanh")

  cgraph::cg.atanh(x)
}
