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
#' @seealso \link[base:Trig]{sin}
#'
#' @author Ron Triepels
#' @export
cg_sin <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sin),
    grads = list(
      x = quote(sin_grad)
    ),
    args = list(x)
  )
}

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
#' @keywords internal
sin_grad <- function(x, val, grad)
{
  grad * cos(x)
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
#' @seealso \link[base:Trig]{cos}
#'
#' @author Ron Triepels
#' @export
cg_cos <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(cos),
    grads = list(
      quote(cos_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
cos_grad <- function(x, val, grad)
{
  -grad * sin(x)
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
#' @seealso \link[base:Trig]{tan}
#'
#' @author Ron Triepels
#' @export
cg_tan <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(tan),
    grads = list(
      quote(tan_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
tan_grad <- function(x, val, grad)
{
  grad / cos(x)^2
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
#' @seealso \link[base:Hyperbolic]{sinh}
#'
#' @author Ron Triepels
#' @export
cg_sinh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(sinh),
    grads = list(
      quote(sinh_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
sinh_grad <- function(x, val, grad)
{
  grad * cosh(x)
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
#' @seealso \link[base:Hyperbolic]{cosh}
#'
#' @author Ron Triepels
#' @export
cg_cosh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(cosh),
    grads = list(
      quote(cosh_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
cosh_grad <- function(x, val, grad)
{
  grad * sinh(x)
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
#' @seealso \link[base:Hyperbolic]{tanh}
#'
#' @author Ron Triepels
#' @export
cg_tanh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(tanh),
    grads = list(
      quote(tanh_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
tanh_grad <- function(x, val, grad)
{
  grad * (1 - val^2)
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
#' @seealso \link[base:Trig]{asin}
#'
#' @author Ron Triepels
#' @export
cg_asin <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(asin),
    grads = list(
      quote(asin_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
asin_grad <- function(x, val, grad)
{
  grad / sqrt(1 - x^2)
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
#' @seealso \link[base:Trig]{acos}
#'
#' @author Ron Triepels
#' @export
cg_acos <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(acos),
    grads = list(
      quote(acos_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
acos_grad <- function(x, val, grad)
{
  -grad / sqrt(1 - x^2)
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
#' @seealso \link[base:Trig]{atan}
#'
#' @author Ron Triepels
#' @export
cg_atan <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(atan),
    grads = list(
      quote(atan_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
atan_grad <- function(x, val, grad)
{
  grad / (x^2 + 1)
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
#' @seealso \link[base:Hyperbolic]{asinh}
#'
#' @author Ron Triepels
#' @export
cg_asinh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(asinh),
    grads = list(
      quote(asinh_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
asinh_grad <- function(x, val, grad)
{
  grad / sqrt(x^2 + 1)
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
#' @seealso \link[base:Hyperbolic]{acosh}
#'
#' @author Ron Triepels
#' @export
cg_acosh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(acosh),
    grads = list(
      quote(acosh_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
acosh_grad <- function(x, val, grad)
{
  grad / sqrt(x^2 - 1)
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
#' @seealso \link[base:Hyperbolic]{atanh}
#'
#' @author Ron Triepels
#' @export
cg_atanh <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(atanh),
    grads = list(
      quote(atanh_grad)
    ),
    args = list(x)
  )
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
#' @keywords internal
atanh_grad <- function(x, val, grad)
{
  grad / (1 - x^2)
}
