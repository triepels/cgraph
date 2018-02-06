#' Matrix Multiplication
#'
#' Calculate \code{x \%*\% y}.
#'
#' @section Usage:
#' \preformatted{mat.mul(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{y}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name mat.mul
#' @author Ron Triepels
cgraph$public_methods$mat.mul <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(x %*% y),
    grads = list(
      x = quote(tcrossprod(grad, y)),
      y = quote(crossprod(x, grad))
    ),
    binding = list(x = x, y = y)
  )
}

#' Matrix Crossproduct
#'
#' Calculate \code{crossprod(x, y)}.
#'
#' @section Usage:
#' \preformatted{crossprod(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{y}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note In contrast to the original \code{crossprod} function, this function requires both \code{x} and \code{y} to be supplied.
#'
#' @return symbol, name of the operation.
#'
#' @name crossprod
#' @author Ron Triepels
cgraph$public_methods$crossprod <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(crossprod(x, y)),
    grads = list(
      x = quote(y %*% grad),
      y = quote(x %*% grad)
    ),
    binding = list(x = x, y = y)
  )
}

#' Transpose Matrix Crossproduct
#'
#' Calculate \code{tcrossprod(x, y)}.
#'
#' @section Usage:
#' \preformatted{tcrossprod(x, y, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{y}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note In contrast to the original \code{tcrossprod} function, this function requires both \code{x} and \code{y} to be supplied.
#'
#' @return symbol, name of the operation.
#'
#' @name tcrossprod
#' @author Ron Triepels
cgraph$public_methods$tcrossprod <- function(x, y, name = self$name())
{
  self$expr(name = name,
    call = quote(tcrossprod(x, y)),
    grads = list(
      x = quote(grad %*% y),
      y = quote(grad %*% x)
    ),
    binding = list(x = x, y = y)
  )
}

#' Linear Transformation
#'
#' Calculate \code{x \%*\% y + c(z)}.
#'
#' @section Usage:
#' \preformatted{linear(x, y, z, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{y}{character scalar or symbol, placeholder for a numeric matrix.}
#' \item{z}{character scalar or symbol, placeholder for a numeric vector.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note This function is equivalent to \code{add(mat.mul(x, y), c(z))}.
#'
#' @return symbol, name of the operation.
#'
#' @name linear
#' @author Ron Triepels
cgraph$public_methods$linear <- function(x, y, z, name = self$name())
{
  self$expr(name = name,
    call = quote(x %*% y + c(z)),
    grads = list(
      x = quote(tcrossprod(grad, y)),
      y = quote(crossprod(x, grad)),
      z = quote(matrix(rowSums(grad)))
    ),
    binding = list(x = x, y = y, z = z)
  )
}

#' Sum of Vector Elements
#'
#' Calculate \code{sum(x)}.
#'
#' @section Usage:
#' \preformatted{sum(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note In contrast to the original \code{sum} function, this function is not vectorized and only sums the elements of a single array.
#'
#' @return symbol, name of the operation.
#'
#' @name sum
#' @author Ron Triepels
cgraph$public_methods$sum <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(sum(x)),
    grads = list(x = quote(array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Row Sums
#'
#' Calculate \code{rowSums(x)}.
#'
#' @section Usage:
#' \preformatted{rowSums(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array of two or more dimensions.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name rowSums
#' @author Ron Triepels
cgraph$public_methods$rowSums <- function(x, name = self$name())
{
 self$expr(name = name,
   call = quote(rowSums(x)),
   grads = list(x = quote(array(grad, dim(x)))),
   binding = list(x = x)
 )
}

#' Column Sums
#'
#' Calculate \code{colSums(x)}.
#'
#' @section Usage:
#' \preformatted{colSums(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array of two or more dimensions.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name colSums
#' @author Ron Triepels
cgraph$public_methods$colSums <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(colSums(x)),
    grads = list(x = quote(t(array(grad, dim(x))))),
    binding = list(x = x)
  )
}

#' Arithmetic Mean
#'
#' Calculate \code{sum(x) / length(x)}.
#'
#' @section Usage:
#' \preformatted{mean(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @note For computational efficiency, this function does not use the standard \code{mean} function.
#'
#' @return symbol, name of the operation.
#'
#' @name mean
#' @author Ron Triepels
cgraph$public_methods$mean <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(sum(x) / length(x)),
    grads = list(x = quote(1 / length(x) * array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Row Means
#'
#' Calculate \code{rowMeans(x)}.
#'
#' @section Usage:
#' \preformatted{rowMeans(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array of two or more dimensions.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name rowMeans
#' @author Ron Triepels
cgraph$public_methods$rowMeans <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(rowMeans(x)),
    grads = list(x = quote(1 / prod(dim(x)[-1]) * array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Column Means
#'
#' Calculate \code{colMeans(x)}.
#'
#' @section Usage:
#' \preformatted{colMeans(x, name)}
#'
#' @section Agruments:
#' \describe{
#' \item{x}{character scalar or symbol, placeholder for a numeric array of two or more dimensions.}
#' \item{name}{character scalar, name of the operation (optional).}
#' }
#'
#' @return symbol, name of the operation.
#'
#' @name colMeans
#' @author Ron Triepels
cgraph$public_methods$colMeans <- function(x, name = self$name())
{
  self$expr(name = name,
    call = quote(colMeans(x)),
    grads = list(x = quote(1 / dim(x)[1] * array(grad, dim(x)))),
    binding = list(x = x)
  )
}
