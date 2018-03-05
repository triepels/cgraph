#' Add
#'
#' Calculate \code{x + y}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param y cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.add <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x + y),
    grads = list(x = quote(grad), y = quote(grad)),
    binding = list(x = x, y = y)
  )
}

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pos <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x),
    grads = list(x = quote(grad)),
    binding = list(x = x)
  )
}

`+.cg.node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg.pos(x)
  }
  else
  {
    cgraph::cg.add(x, y)
  }
}

#' Subtract
#'
#' Calculate \code{x - y}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param y cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sub <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x - y),
    grads = list(x = quote(grad), y = quote(-grad)),
    binding = list(x = x, y = y)
  )
}

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.neg <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(-x),
    grads = list(x = quote(-grad)),
    binding = list(x = x)
  )
}

`-.cg.node` <- function(x, y)
{
  if(missing(y))
  {
    cgraph::cg.neg(x)
  }
  else
  {
    cgraph::cg.sub(x, y)
  }
}

#' Multiply
#'
#' Calculate \code{x * y} element-wise.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param y cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.mul <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x * y),
    grads = list(x = quote(grad * y), y = quote(grad * x)),
    binding = list(x = x, y = y)
  )
}

`*.cg.node` <- function(x, y)
{
  cgraph::cg.mul(x, y)
}

#' Divide
#'
#' Calculate \code{x / y}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param y cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.div <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x / y),
    grads = list(x = quote(grad * 1 / y), y = quote(grad * x / y^2)),
    binding = list(x = x, y = y)
  )
}

`/.cg.node` <- function(x, y)
{
  cgraph::cg.div(x, y)
}

#' Power
#'
#' Calculate \code{x^y}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param y cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pow <- function(x, y, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x^y),
    grads = list(x = quote(grad * y * x^(y - 1)), y = quote(grad * x^y * log(x))),
    binding = list(x = x, y = y)
  )
}

`^.cg.node` <- function(x, y)
{
  cgraph::cg.pow(x, y)
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sqrt <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(x^0.5),
    grads = list(x = quote(grad * 0.5 * x^-1.5)),
    binding = list(x = x)
  )
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
sqrt.cg.node <- function(x)
{
  cgraph::cg.sqrt(x)
}

#' Exponential Function
#'
#' Calculate \code{exp(1)^x}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @note \code{exp(1)=e}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.exp <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(exp(1)^x),
    grads = list(x = quote(grad * exp(1)^x)),
    binding = list(x = x)
  )
}

#' Exponential Function
#'
#' Calculate \code{exp(1)^x}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#'
#' @note \code{exp(1)=e}.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
exp.cg.node <- function(x)
{
  cgraph::cg.exp(x)
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.abs <- function(x, name = cgraph::name())
{
  cgraph::expr(name = name,
    call = quote(abs(x)),
    grads = list(x = quote(grad * (x / abs(x)))),
    binding = list(x = x)
  )
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric scalar or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
abs.cg.node <- function(x)
{
  cgraph::cg.abs(x)
}
