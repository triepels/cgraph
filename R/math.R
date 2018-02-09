#' Negative
#'
#' Calculate \code{-x}.
#'
#' @section Usage:
#' \preformatted{negative(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name negative
#' @author Ron Triepels
cgraph$public_methods$negative <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(-x),
    grads = list(x = quote(-grad)),
    binding = list(x = x)
  )
}

#' Add
#'
#' Calculate \code{x + y}.
#'
#' @section Usage:
#' \preformatted{add(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{y}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name add
#' @author Ron Triepels
cgraph$public_methods$add <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(x + y),
    grads = list(x = quote(grad), y = quote(grad)),
    binding = list(x = x, y = y)
  )
}

#' Subtract
#'
#' Calculate \code{x - y}.
#'
#' @section Usage:
#' \preformatted{subtract(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{y}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name subtract
#' @author Ron Triepels
cgraph$public_methods$subtract <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(x - y),
    grads = list(x = quote(grad), y = quote(-grad)),
    binding = list(x = x, y = y)
  )
}

#' Multiply
#'
#' Calculate \code{x * y}.
#'
#' @section Usage:
#' \preformatted{multiply(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{y}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name multiply
#' @author Ron Triepels
cgraph$public_methods$multiply <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(x * y),
    grads = list(x = quote(grad * y), y = quote(grad * x)),
    binding = list(x = x, y = y)
  )
}

#' Power
#'
#' Calculate \code{x^y}.
#'
#' @section Usage:
#' \preformatted{pow(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{y}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name pow
#' @author Ron Triepels
cgraph$public_methods$pow <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(x^y),
    grads = list(x = quote(grad * y * x^(y - 1)), y = quote(grad * x^y * log(x))),
    binding = list(x = x, y = y)
  )
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @section Usage:
#' \preformatted{sqrt(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name sqrt
#' @author Ron Triepels
cgraph$public_methods$sqrt <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(x^2),
    grads = list(x = quote(grad * 2 * x)),
    binding = list(x = x)
  )
}


#' Exponential Function
#'
#' Calculate \code{exp(1)^x}.
#'
#' @section Usage:
#' \preformatted{exp(x, name)}
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
#' @name exp
#' @author Ron Triepels
cgraph$public_methods$exp <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(exp(1)^x),
    grads = list(x = quote(grad * exp(1)^x)),
    binding = list(x = x)
  )
}

#' Natural Log
#'
#' Calculate \code{log(x, base = exp(1))}.
#'
#' @section Usage:
#' \preformatted{ln(x, y, name)}
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
#' @name ln
#' @author Ron Triepels
cgraph$public_methods$ln <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(log(x)),
    grads = list(x = quote(grad * 1 / x)),
    binding = list(x = x)
  )
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @section Usage:
#' \preformatted{abs(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric scalar or array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name abs
#' @author Ron Triepels
cgraph$public_methods$abs <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(abs(x)),
    grads = list(x = quote(grad * (x / abs(x)))),
    binding = list(x = x)
  )
}
