#' Sigmoid
#'
#' Calculate \code{1 / (1 + exp(-x))}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sigmoid <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(1 / (1 + exp(-x))),
    grads = list(x = quote(cg.sigmoid.grad(y, grad))),
    binding = list(x = x, y = name)
  )
}

#' Sigmoid Gradient
#'
#' Calculate the gradient of \code{1 / (1 + exp(-x))} with respect to \code{x}.
#'
#' @param y numeric vector or array, value of \code{1 / (1 + exp(-x))}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
cg.sigmoid.grad <- function(y, grad)
{
  grad * y * (1 - y)
}
