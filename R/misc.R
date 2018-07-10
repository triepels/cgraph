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
  cgraph::expr(name = name,
    call = quote(1 / (1 + exp(-x))),
    grads = list(x = quote(sigmoid.grad(y, grad))),
    binding = list(x = x, y = name)
  )
}

# Export gradient
export("sigmoid.grad", function(y, grad)
{
  grad * y * (1 - y)
})
