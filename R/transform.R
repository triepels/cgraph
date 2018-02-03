#' Reshape Array Dimensions
#'
#' Change the dimensions of array \code{x} to \code{dims}.
#'
#' @section Usage:
#' \preformatted{reshape(x, dims, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{dims}{numeric scalar or vector, the dimensions of the new array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note The elements of \code{x} are rearranged column-wise.
#'
#' @return symbol, name of the operation.
#'
#' @name reshape
#' @author Ron Triepels
cgraph$public_methods$reshape <- function(x, dims, name = self$name())
{
  self$expr(name = name,
    call = substitute(array(x, dims), list(dims = dims)),
    grads = list(x = quote(array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Transform Array to Vector
#'
#' Transform array \code{x} to a one-dimensional vector.
#'
#' @section Usage:
#' \preformatted{c(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name c
#' @author Ron Triepels
cgraph$public_methods$c <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(c(x)),
    grads = list(x = quote(array(x, dim(x)))),
    binding = list(x = x)
  )
}

#' Matrix Transpose
#'
#' Calculate \code{t(x)}.
#'
#' @section Usage:
#' \preformatted{t(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name t
#' @author Ron Triepels
cgraph$public_methods$t <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(t(x)),
    grads = list(x = quote(t(grad))),
    binding = list(x = x)
  )
}
