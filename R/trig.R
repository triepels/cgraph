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
    grads = list(x = quote(grad * cos(x))),
    binding = list(x = x)
  )
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
  cgraph::expr(name = name,
    call = quote(cos(x)),
    grads = list(x = quote(-grad * sin(x))),
    binding = list(x = x)
  )
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
  cgraph::expr(name = name,
    call = quote(tan(x)),
    grads = list(x = quote(grad / cos(x)^2)),
    binding = list(x = x)
  )
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
  cgraph::expr(name = name,
    call = quote(tanh(x)),
    grads = list(x = quote(grad * (1 - y^2))),
    binding = list(x = x, y = name)
  )
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
