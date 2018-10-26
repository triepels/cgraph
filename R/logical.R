#' Not
#'
#' Calculate \code{!x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.not <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(`!`),
    grads = list(
      quote(constant.grad)
    ),
    args = list(x)
  )
}

#' Logical Gradient
#'
#' Calculate the gradient of logical \code{x}.
#'
#' @param x numeric vector or array, value of \code{x}.
#'
#' @return numeric vector or array, gradient of the operation.
#'
#' @author Ron Triepels
#' @keywords internal
not.grad <- function(x, val, grad)
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

# S3 method
`!.cg.node` <- function(x)
{
  cgraph::cg.not(x)
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
#' @author Ron Triepels
cg.equal <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`==`),
    grads = list(
      quote(equal.grad.x),
      quote(equal.grad.y)
    ),
    args = list(x, y)
  )
}

equal.grad.x <- function(x, y, val, grad)
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

equal.grad.y <- function(x, y, val, grad)
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

# S3 method
`==.cg.node` <- function(x, y)
{
  cgraph::cg.equal(x, y)
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
#' @author Ron Triepels
cg.not.equal <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`!=`),
    grads = list(
      quote(not.equal.grad.x),
      quote(not.equal.grad.y)
    ),
    args = list(x, y)
  )
}

not.equal.grad.x <- function(x, y, val, grad)
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

not.equal.grad.y <- function(x, y, val, grad)
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

# S3 method
`!=.cg.node` <- function(x, y)
{
  cgraph::cg.not.equal(x, y)
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
#' @author Ron Triepels
cg.less <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`<`),
    grads = list(
      quote(less.grad.x),
      quote(less.grad.y)
    ),
    args = list(x, y)
  )
}

less.grad.x <- function(x, y, val, grad)
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

less.grad.y <- function(x, y, val, grad)
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

# S3 method
`<.cg.node` <- function(x, y)
{
  cgraph::cg.less(x, y)
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
#' @author Ron Triepels
cg.greater <- function(x, y, name)
{
  cgraph::opr(name = name,
    call = quote(`>`),
    grads = list(
      quote(greater.grad.x),
      quote(greater.grad.y)
    ),
    args = list(x, y)
  )
}

greater.grad.x <- function(x, y, val, grad)
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

greater.grad.y <- function(x, y, val, grad)
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

# S3 method
`>.cg.node` <- function(x, y)
{
  cgraph::cg.greater(x, y)
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
#' @author Ron Triepels
cg.less.equal <- function(x, y, name)
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

less.equal.grad.x <- function(x, y, val, grad)
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

less.equal.grad.y <- function(x, y, val, grad)
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

# S3 method
`<=.cg.node` <- function(x, y)
{
  cgraph::cg.less.equal(x, y)
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
#' @author Ron Triepels
cg.greater.equal <- function(x, y, name)
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

greater.equal.grad.x <- function(x, y, val, grad)
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

greater.equal.grad.y <- function(x, y, val, grad)
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

# S3 method
`>=.cg.node` <- function(x, y)
{
  cgraph::cg.greater.equal(x, y)
}
