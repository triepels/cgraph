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
#' Calculate \code{x \%*\% y + c(z)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param z cg.node, placeholder for a numeric vector.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is equivalent to \code{x \%mul\% y + cg.c(z)}.
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
    grads = list(x = quote(array(grad, dim(x)))),
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
    grads = list(x = quote(perm(array(grad, dim(x))))),
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
  cgraph::expr(name = name,
    call = quote(sum(x) / length(x)),
    grads = list(x = quote(1 / length(x) * array(grad, dim(x)))),
    binding = list(x = x)
  )
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
    grads = list(x = quote(1 / prod(dim(x)[-1]) * array(grad, dim(x)))),
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
    grads = list(x = quote(1 / dim(x)[1] * perm(array(grad, dim(x))))),
    binding = list(x = x)
  )
}
