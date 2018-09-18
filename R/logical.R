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
cg.not <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(!x),
    grads = list(x = quote(cg.constant.grad(x))),
    binding = list(x = x)
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
cg.constant.grad <- function(x)
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
cg.equal <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x == y),
    grads = list(
      x = quote(cg.constant.grad(x)),
      y = quote(cg.constant.grad(y))
    ),
    binding = list(x = x, y = y)
  )
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
cg.not.equal <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x != y),
    grads = list(
      x = quote(cg.constant.grad(x)),
      y = quote(cg.constant.grad(y))
    ),
    binding = list(x = x, y = y)
  )
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
cg.less <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x < y),
    grads = list(
      x = quote(cg.constant.grad(x)),
      y = quote(cg.constant.grad(y))
    ),
    binding = list(x = x, y = y)
  )
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
cg.greater <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x > y),
    grads = list(
      x = quote(cg.constant.grad(x)),
      y = quote(cg.constant.grad(y))
    ),
    binding = list(x = x, y = y)
  )
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
cg.less.equal <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x <= y),
    grads = list(
      x = quote(cg.constant.grad(x)),
      y = quote(cg.constant.grad(y))
    ),
    binding = list(x = x, y = y)
  )
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
cg.greater.equal <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x >= y),
    grads = list(
      x = quote(cg.constant.grad(x)),
      y = quote(cg.constant.grad(y))
    ),
    binding = list(x = x, y = y)
  )
}

# S3 method
`>=.cg.node` <- function(x, y)
{
  cgraph::cg.greater.equal(x, y)
}
