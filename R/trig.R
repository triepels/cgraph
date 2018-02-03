#' Sinus
#'
#' Calculate \code{sin(x)}.
#'
#' @section Usage:
#' \preformatted{sin(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name sin
#' @author Ron Triepels
cgraph$public_methods$sin <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(sin(x)),
    grads = list(x = quote(grad * cos(x))),
    binding = list(x = x)
  )
}

#' Cosinus
#'
#' Calculate \code{cos(x)}.
#'
#' @section Usage:
#' \preformatted{cos(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name cos
#' @author Ron Triepels
cgraph$public_methods$cos <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(cos(x)),
    grads = list(x = quote(-grad * sin(x))),
    binding = list(x = x)
  )
}

#' Tangent
#'
#' Calculate \code{tan(x)}.
#'
#' @section Usage:
#' \preformatted{tan(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name tan
#' @author Ron Triepels
cgraph$public_methods$tan <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(tan(x)),
    grads = list(x = quote(grad / cos(x)^2)),
    binding = list(x = x)
  )
}

#' Hyperbolic Tangent
#'
#' Calculate \code{tanh(x)}.
#'
#' @section Usage:
#' \preformatted{tanh(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name tanh
#' @author Ron Triepels
cgraph$public_methods$tanh <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(tanh(x)),
    grads = list(x = quote(grad * (1 - y^2))),
    binding = list(x = x, y = name)
  )
}
