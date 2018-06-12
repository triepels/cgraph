# Load packages
library(cgraph)

#' Check If Two Numerical Arrays Are (Nearly) Equal
#'
#' Determines if abs(\code{x}-\code{y}) is smaller than \code{tol}.
#'
#' @param x numerical array
#' @param y numerical array
#' @param tol numerical scalar, the maximum tolerance
#'
#' @return logical, TRUE if \code{x} and \code{y} are equal. Otherwise FALSE.
#'
#' @author Ron Triepels
equal <- function(x, y, tol = 1e-6)
{
  return(abs(x - y) <= tol)
}

#'
#' Add, subtract, multiply, and divide
#'

x <- cgraph$new()

a <- parm(2)

b <- parm(4)

c <- (a + b) * (a - b) * (a / b)

values <- x$run(c)

grads <- x$gradients(c, values)

if(!equal(grads$prm1, -1))
{
  stop("Invalid gradient")
}

if(!equal(grads$prm2, -2.5))
{
  stop("Invalid gradient")
}

#'
#' Power and Square Root
#'

x <- cgraph$new()

a <- parm(2)

b <- parm(4)

c <- a^b + sqrt(a)

values <- x$run(c)

grads <- x$gradients(c, values)

if(!equal(grads$prm1, 32 + 1 / (2 * sqrt(2))))
{
  stop("Invalid gradient")
}

if(!equal(grads$prm2, 16 * log(2)))
{
  stop("Invalid gradient")
}

#'
#' Exponent and Logarithm
#'

x <- cgraph$new()

a <- parm(2)

b <- parm(4)

c <- exp(a) + log(a, base = b)

values <- x$run(c)

grads <- x$gradients(c, values)

if(!equal(grads$prm1, exp(1)^2 + 1 / log(16)))
{
  stop("Invalid gradient")
}

if(!equal(grads$prm2, -log(2) / (4 * log(4)^2)))
{
  stop("Invalid gradient")
}

#'
#' Some More Logarithms
#'

x <- cgraph$new()

a <- parm(2)

b <- parm(4)

c <- log2(a) + log10(b) + cg.ln(a)

values <- x$run(c)

grads <- x$gradients(c, values)

if(!equal(grads$prm1, (1 + log(2)) / (2 * log(2))))
{
  stop("Invalid gradient")
}

if(!equal(grads$prm2, 1 / (4 * log(10))))
{
  stop("Invalid gradient")
}

#'
#' Absolute value
#'

x <- cgraph$new()

a <- parm(2)

b <- parm(4)

c <- abs(-a / b)

values <- x$run(c)

grads <- x$gradients(c, values)

if(!equal(grads$prm1, 1 / 4))
{
  stop("Invalid gradient")
}

if(!equal(grads$prm2, -1 / 8))
{
  stop("Invalid gradient")
}
