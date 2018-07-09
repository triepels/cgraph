#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is identical to \code{cg.as.numeric}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.as.double <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(as.numeric(x)),
    grads = list(x = quote(as.double.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("as.double.grad", function(x, grad)
{
  `if`(is.array(x), array(grad, dim(x)), as.numeric(grad))
})

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#' @param ... further arguments passed to or from other methods.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
as.double.cg.node <- function(x, name = cgraph::name(), ...)
{
  cgraph::cg.as.double(x, name)
}

#' Coerce to a Numeric Vector
#'
#' Coerce \code{x} to a one-dimensional numeric vector.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note This function is identical to \code{cg.as.double}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.as.numeric <- function(x, name = cgraph::name())
{
  cgraph::cg.as.double(x, name)
}

#' Reshape Array Dimensions
#'
#' Change the dimensions of array \code{x} to \code{dims}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param dim numeric scalar or vector, the dimensions of the new array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note The elements of \code{x} are re-arranged column-wise.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.reshape <- function(x, dim, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = substitute(array(x, dim), list(dim = dim)),
    grads = list(x = quote(reshape.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("reshape.grad", function(x, grad)
{
  `if`(is.array(x), array(grad, dim(x)), as.numeric(grad))
})

#' Coerce to an Array
#'
#' Coerce \code{x} to an array with dimensions \code{dims}.
#'
#' @param x cg.node, placeholder for a numeric array.
#' @param dims numeric scalar or vector, the dimensions of the new array.
#' @param name character scalar, name of the operation (optional).
#' @param ... further arguments passed to or from other methods.
#'
#' @note This function is equivalent to function \code{cg.reshape}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
as.array.cg.node <- function(x, dims, name = cgraph::name(), ...)
{
  cgraph::cg.reshape(x, dims, name)
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
