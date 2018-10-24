#' Add
#'
#' Calculate \code{x + y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.add <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = as.name("+"),
    grads = list(
      as.name("add.grad"),
      as.name("add.grad")
    ),
    args = list(x, y)
  )
}
