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
  cgraph::expr(name = name,
    call = quote(sin(x)),
    grads = list(x = quote(sin.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
.cg$export("sin.grad", function(x, grad)
{
  grad * cos(x)
})

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
  cgraph::expr(name = name,
    call = quote(cos(x)),
    grads = list(x = quote(cos.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
.cg$export("cos.grad", function(x, grad)
{
  -grad * sin(x)
})

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
  cgraph::expr(name = name,
    call = quote(tan(x)),
    grads = list(x = quote(tan.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
.cg$export("tan.grad", function(x, grad)
{
  grad / cos(x)^2
})

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
  cgraph::expr(name = name,
    call = quote(tanh(x)),
    grads = list(x = quote(tanh.grad(y, grad))),
    binding = list(x = x, y = name)
  )
}

# Export gradient
.cg$export("tanh.grad", function(y, grad)
{
  grad * (1 - y^2)
})

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
