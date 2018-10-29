#' Not
#'
#' Calculate \code{!x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Logic]{not}
#'
#' @author Ron Triepels
#' @export
cg_not <- function(x, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`!`),
    grads = list(
      quote(not_grad)
    ),
    args = list(x)
  )
}

#' Not Gradient
#'
#' Calculate the gradient of \code{!x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param val numeric vector or array, value of \code{!x}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
not_grad <- function(x, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' @export
`!.cg_node` <- function(x)
{
  cgraph::cg_not(x)
}

#' Equal
#'
#' Calculate \code{x == y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{equal}
#'
#' @author Ron Triepels
#' @export
cg_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`==`),
    grads = list(
      quote(equal_grad_x),
      quote(equal_grad_y)
    ),
    args = list(x, y)
  )
}

#' Equal Gradient
#'
#' Calculate the gradient of \code{x == y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x == y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Equal Gradient
#'
#' Calculate the gradient of \code{x == y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x == y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' @export
`==.cg_node` <- function(x, y)
{
  cgraph::cg_equal(x, y)
}

#' Not equal
#'
#' Calculate \code{x != y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{not equal}
#'
#' @author Ron Triepels
#' @export
cg_not_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`!=`),
    grads = list(
      quote(not_equal_grad_x),
      quote(not_equal_grad_y)
    ),
    args = list(x, y)
  )
}

#' Not Equal Gradient
#'
#' Calculate the gradient of \code{x != y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x != y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
not_equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Not Equal Gradient
#'
#' Calculate the gradient of \code{x != y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x != y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
not_equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' @export
`!=.cg_node` <- function(x, y)
{
  cgraph::cg_not_equal(x, y)
}

#' Less
#'
#' Calculate \code{x < y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{less}
#'
#' @author Ron Triepels
#' @export
cg_less <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`<`),
    grads = list(
      quote(less_grad_x),
      quote(less_grad_y)
    ),
    args = list(x, y)
  )
}

#' Less Gradient
#'
#' Calculate the gradient of \code{x < y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x < y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
less_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Less Gradient
#'
#' Calculate the gradient of \code{x < y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x < y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
less_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' @export
`<.cg_node` <- function(x, y)
{
  cgraph::cg_less(x, y)
}

#' Greater
#'
#' Calculate \code{x > y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{greater}
#'
#' @author Ron Triepels
#' @export
cg_greater <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`>`),
    grads = list(
      quote(greater_grad_x),
      quote(greater_grad_y)
    ),
    args = list(x, y)
  )
}

#' Greater Gradient
#'
#' Calculate the gradient of \code{x > y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x > y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
greater_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Greater Gradient
#'
#' Calculate the gradient of \code{x > y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x > y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
greater_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' @export
`>.cg_node` <- function(x, y)
{
  cgraph::cg_greater(x, y)
}

#' Less or Equal
#'
#' Calculate \code{x <= y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{less or equal}
#'
#' @author Ron Triepels
#' @export
cg_less_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`<=`),
    grads = list(
      quote(less.equal.grad.x),
      quote(less.equal.grad.y)
    ),
    args = list(x, y)
  )
}

#' Less or Equal Gradient
#'
#' Calculate the gradient of \code{x <= y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x <= y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
less_equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Less or Equal Gradient
#'
#' Calculate the gradient of \code{x <= y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x <= y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
less_equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' @export
`<=.cg_node` <- function(x, y)
{
  cgraph::cg_less_equal(x, y)
}

#' Greater or Equal
#'
#' Calculate \code{x >= y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @seealso \link[base:Comparison]{greater or equal}
#'
#' @author Ron Triepels
#' @export
cg_greater_equal <- function(x, y, name = NULL)
{
  cgraph::opr(name = name,
    call = quote(`>=`),
    grads = list(
      quote(greater.equal.grad.x),
      quote(greater.equal.grad.y)
    ),
    args = list(x, y)
  )
}

#' Greater or Equal Gradient
#'
#' Calculate the gradient of \code{x >= y} with respect to \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x >= y}.
#' @param grad numeric vector or array, gradient of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
greater_equal_grad_x <- function(x, y, val, grad)
{
  if(is.array(x))
  {
    array(0, dim(x))
  }
  else
  {
    rep(0, length(x))
  }
}

#' Greater or Equal Gradient
#'
#' Calculate the gradient of \code{x >= y} with respect to \code{y}.
#'
#' @param x numeric vector or array, value of \code{x}.
#' @param y numeric vector or array, value of \code{y}.
#' @param val numeric vector or array, value of \code{x >= y}.
#' @param grad numeric vector or array, gradient of \code{y}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
greater_equal_grad_y <- function(x, y, val, grad)
{
  if(is.array(y))
  {
    array(0, dim(y))
  }
  else
  {
    rep(0, length(y))
  }
}

#' @export
`>=.cg_node` <- function(x, y)
{
  cgraph::cg_greater_equal(x, y)
}
