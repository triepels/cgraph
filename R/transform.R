#' Reshape Array Dimensions
#'
#' Change the dimensions of array \code{x} to \code{dims}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param dims numeric scalar or vector, the dimensions of the new array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note The elements of \code{x} are rearranged column-wise.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.reshape <- function(x, dims, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = substitute(array(x, dims), list(dims = dims)),
    grads = list(x = quote(array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Transform Array to Vector
#'
#' Transform array \code{x} to a one-dimensional vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.c <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(c(x)),
    grads = list(x = quote(array(grad, dim(x)))),
    binding = list(x = x)
  )
}

#' Matrix Transpose
#'
#' Perform \code{t(x)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.t <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(t(x)),
    grads = list(x = quote(t(grad))),
    binding = list(x = x)
  )
}

#' Matrix Transpose
#'
#' Perform \code{t(x)}.
#'
#' @param x cg.node, placeholder for a numeric matrix.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
t.cg.node <- function(x)
{
  cgraph::cg.t(x)
}
