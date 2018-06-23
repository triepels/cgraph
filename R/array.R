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
cg.matmul <- function(x, y, name = cgraph::name())
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

#' Matrix Crossproduct
#'
#' Calculate \code{crossprod(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param y cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{crossprod} function, this function requires both \code{x} and \code{y} to be supplied.
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
#' @note In contrast to the base \code{tcrossprod} function, this function requires both \code{x} and \code{y} to be supplied.
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
      z = quote(bsum(grad, length(z)))
    ),
    binding = list(x = x, y = y, z = z)
  )
}

#' Sum of Vector Elements
#'
#' Calculate \code{sum(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{sum} function, this function only accepts a single variable.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sum <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(sum(x)),
    grads = list(x = quote(`if`(is.array(x), array(grad, dim(x)), rep_len(grad, length(x))))),
    binding = list(x = x)
  )
}

#' Product of Vector Elements
#'
#' Calculate \code{prod(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note In contrast to the base \code{prod} function, this function only accepts a single variable.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.prod <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(prod(x)),
    grads = list(x = quote(prod(x) / x)),
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
#' @param x cg.node, placeholder for a numeric vector or array.
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
    grads = list(x = quote(1 / length(x) * `if`(is.array(x), array(grad, dim(x)), rep_len(grad, length(x))))),
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
    grads = list(x = quote(1 / dim(x)[1] * aperm(array(grad, rev(dim(x)))))),
    binding = list(x = x)
  )
}

#' Maxima
#'
#' Calculate \code{max(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
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
#' @param x cg.node, placeholder for a numeric vector or array.
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

#' Parallel Maxima
#'
#' Calculate \code{pmax(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pmax <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(pmax(x, y)),
    grads = list(
      x = quote(`if`(is.array(x), grad * (x >= y), bsum(grad * (x >= y), length(x)))),
      y = quote(`if`(is.array(y), grad * (x < y), bsum(grad * (x < y), length(y))))
    ),
    binding = list(x = x, y = y)
  )
}

#' Parallel Minima
#'
#' Calculate \code{pmin(x, y)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pmin <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(pmin(x, y)),
    grads = list(
      x = quote(`if`(is.array(x), grad * (x <= y), bsum(grad * (x <= y), length(x)))),
      y = quote(`if`(is.array(y), grad * (x > y), bsum(grad * (x > y), length(y))))
    ),
    binding = list(x = x, y = y)
  )
}
