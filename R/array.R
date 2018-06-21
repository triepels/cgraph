#' Matrix Multiplication
#'
#' Calculate \code{x \%*\% y}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.mat.mul <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x %*% y),
    grads = list(
      x = quote(tcrossprod(grad, y)),
      y = quote(crossprod(x, grad))
    ),
    binding = list(x = x, y = y)
  )
}

#' Matrix Multiplication
#'
#' Calculate \code{x \%*\% y}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
`%mul%` <- function(x, y)
{
  cgraph::cg.mat.mul(x, y)
}

#' Matrix Crossproduct
#'
#' Calculate \code{crossprod(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the original \code{crossprod} function, this function requires both \code{x} and \code{y} to be supplied.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.crossprod <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
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
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the original \code{tcrossprod} function, this function requires both \code{x} and \code{y} to be supplied.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.tcrossprod <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
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
#' Calculate \code{x \%*\% y + as.numeric(z)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param z cg.node, placeholder for a numeric vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is equivalent to \code{x \%mul\% y + as.numeric(z)}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.linear <- function(x, y, z, name = cgraph::name())
{
  cgraph::expr(name = name,
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
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the original \code{sum} function, this function does not accept a variable amount of arguments.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sum <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(sum(x)),
    grads = list(x = quote(array(grad, `if`(is.array(x), dim(x), length(x))))),
    binding = list(x = x)
  )
}

#' Row Sums
#'
#' Calculate \code{rowSums(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.rowSums <- function(x, name = cgraph::name())
{
 cgraph::expr(name = name,
   call = quote(rowSums(x)),
   grads = list(x = quote(array(grad, dim(x)))),
   binding = list(x = x)
 )
}

#' Column Sums
#'
#' Calculate \code{colSums(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.colSums <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(colSums(x)),
    grads = list(x = quote(aperm(array(grad, rev(dim(x)))))),
    binding = list(x = x)
  )
}

#' Arithmetic Mean
#'
#' Calculate \code{sum(x) / length(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note For computational efficiency, this function does not use the standard \code{mean} function.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.mean <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(sum(x) / length(x)),
    grads = list(x = quote(1L / length(x) * array(grad, `if`(is.array(x), dim(x), length(x))))),
    binding = list(x = x)
  )
}

#' Arithmetic Mean
#'
#' Calculate \code{sum(x) / length(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#' @param ... further arguments passed to or from other methods.
#'
#' @note For computational efficiency, this function does not use the standard \code{mean} function.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
mean.cg.node <- function(x, name = cgraph::name(), ...)
{
  cgraph::cg.mean(x, name)
}

#' Row Means
#'
#' Calculate \code{rowMeans(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.rowMeans <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(rowMeans(x)),
    grads = list(x = quote(1L / prod(dim(x)[-1L]) * array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Column Means
#'
#' Calculate \code{colMeans(x)}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.colMeans <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(colMeans(x)),
    grads = list(x = quote(1L / dim(x)[1L] * aperm(array(grad, rev(dim(x)))))),
    binding = list(x = x)
  )
}

#' Maxima
#'
#' Calculate \code{max(x)}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.max <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(max(x)),
    grads = list(x = quote(c(grad) * (x == c(y)))),
    binding = list(x = x, y = name)
  )
}

#' Minima
#'
#' Calculate \code{min(x)}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.min <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(min(x)),
    grads = list(x = quote(c(grad) * (x == c(y)))),
    binding = list(x = x, y = name)
  )
}
