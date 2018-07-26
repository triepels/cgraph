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
cg.sin <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(sin(x)),
    grads = list(x = quote(cg.sin.grad(x, grad))),
    binding = list(x = x)
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
cg.sin.grad <- function(x, grad)
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
cg.cos <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(cos(x)),
    grads = list(x = quote(cg.cos.grad(x, grad))),
    binding = list(x = x)
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
cg.cos.grad <- function(x, grad)
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
cg.tan <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(tan(x)),
    grads = list(x = quote(cg.tan.grad(x, grad))),
    binding = list(x = x)
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
cg.tan.grad <- function(x, grad)
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
cg.sinh <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(sinh(x)),
    grads = list(x = quote(cg.sinh.grad(x, grad))),
    binding = list(x = x)
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
cg.sinh.grad <- function(x, grad)
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
cg.cosh <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(cosh(x)),
    grads = list(x = quote(cg.cosh.grad(x, grad))),
    binding = list(x = x)
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
cg.cosh.grad <- function(x, grad)
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
cg.tanh <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(tanh(x)),
    grads = list(x = quote(cg.tanh.grad(y, grad))),
    binding = list(x = x, y = name)
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
cg.tanh.grad <- function(y, grad)
{
  grad * (1 - y^2)
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
cg.asin <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(asin(x)),
    grads = list(x = quote(cg.asin.grad(x, grad))),
    binding = list(x = x)
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
cg.asin.grad <- function(x, grad)
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
cg.acos <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(acos(x)),
    grads = list(x = quote(cg.acos.grad(x, grad))),
    binding = list(x = x)
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
cg.acos.grad <- function(x, grad)
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
cg.atan <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(atan(x)),
    grads = list(x = quote(cg.atan.grad(x, grad))),
    binding = list(x = x)
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
cg.atan.grad <- function(x, grad)
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
cg.asinh <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(asinh(x)),
    grads = list(x = quote(cg.asinh.grad(x, grad))),
    binding = list(x = x)
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
cg.asinh.grad <- function(x, grad)
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
cg.acosh <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(acosh(x)),
    grads = list(x = quote(cg.acosh.grad(x, grad))),
    binding = list(x = x)
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
cg.acosh.grad <- function(x, grad)
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
cg.atanh <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(atanh(x)),
    grads = list(x = quote(cg.atanh.grad(x, grad))),
    binding = list(x = x)
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
cg.atanh.grad <- function(x, grad)
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
  cgraph::cg.atanh(x)
}
