#' Sigmoid
#'
#' Calculate \code{1 / (1 + exp(1)^-x)}.
#'
#' @section Usage:
#' \preformatted{sigmoid(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note \code{exp(1)=e}.
#'
#' @return symbol, name of the operation.
#'
#' @name sigmoid
#' @author Ron Triepels
cgraph$public_methods$sigmoid <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(1 / (1 + exp(1)^-x)),
    grads = list(x = quote(grad * y * (1 - y))),
    binding = list(x = x, y = name)
  )
}
