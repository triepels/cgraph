#' Sinus
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

#' Sinus Gradient
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

#' Sinus
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

#' Cosinus
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

#' Cosinus Gradient
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

#' Cosinus
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

#' Inverse Sinus
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

#' Inverse Sinus Gradient
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

#' Inverse Sinus
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

#' Inverse Cosinus
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

#' Inverse Cosinus Gradient
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

#' Inverse Cosinus
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
