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
cg.add <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x + y),
    grads = list(
      x = quote(add.grad(x, grad)),
      y = quote(add.grad(y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

# Export gradient
export("add.grad", function(x, grad)
{
  `if`(is.array(x), grad, bsum(grad, length(x)))
})

#' Positive
#'
#' Calculate \code{x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pos <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x),
    grads = list(x = quote(grad)),
    binding = list(x = x)
  )
}

# S3 method
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
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sub <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x - y),
    grads = list(
      x = quote(sub.grad.x(x, grad)),
      y = quote(sub.grad.y(y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

# Export gradient
export("sub.grad.x", function(x, grad)
{
  `if`(is.array(x), grad, bsum(grad, length(x)))
})

# Export gradient
export("sub.grad.y", function(y, grad)
{
  `if`(is.array(y), -grad, bsum(-grad, length(y)))
})

#' Negative
#'
#' Calculate \code{-x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.neg <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(-x),
    grads = list(x = quote(-grad)),
    binding = list(x = x)
  )
}

# S3 method
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
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.mul <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x * y),
    grads = list(
      x = quote(mul.grad.x(x, y, grad)),
      y = quote(mul.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

# Export gradient
export("mul.grad.x", function(x, y, grad)
{
  `if`(is.array(x), grad * y, bsum(grad * y, length(x)))
})

# Export gradient
export("mul.grad.y", function(x, y, grad)
{
  `if`(is.array(y), grad * x, bsum(grad * x, length(y)))
})

# S3 method
`*.cg.node` <- function(x, y)
{
  cgraph::cg.mul(x, y)
}

#' Divide
#'
#' Calculate \code{x / y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.div <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x / y),
    grads = list(
      x = quote(div.grad.x(x, y, grad)),
      y = quote(div.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

# Export gradient
export("div.grad.x", function(x, y, grad)
{
  `if`(is.array(x), grad / y, bsum(grad / y, length(x)))
})

# Export gradient
export("div.grad.y", function(x, y, grad)
{
  `if`(is.array(y), -grad * x / y^2, bsum(-grad * x / y^2, length(y)))
})

# S3 method
`/.cg.node` <- function(x, y)
{
  cgraph::cg.div(x, y)
}

#' Power
#'
#' Calculate \code{x^y}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param y cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.pow <- function(x, y, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(x^y),
    grads = list(
      x = quote(pow.grad.x(x, y, grad)),
      y = quote(pow.grad.y(x, y, grad))
    ),
    binding = list(x = x, y = y)
  )
}

# Export gradient
export("pow.grad.x", function(x, y, grad)
{
  `if`(is.array(x), grad * y * x^(y - 1), bsum(grad * y * x^(y - 1), length(x)))
})

# Export gradient
export("pow.grad.y", function(x, y, grad)
{
  `if`(is.array(y), grad * x^y * log(x), bsum(grad * x^y * log(x), length(y)))
})

# S3 method
`^.cg.node` <- function(x, y)
{
  cgraph::cg.pow(x, y)
}

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.sqrt <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(sqrt(x)),
    grads = list(x = quote(sqrt.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("sqrt.grad", function(x, grad)
{
  grad * 1 / (2 * sqrt(x))
})

#' Square Root
#'
#' Calculate \code{sqrt(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
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
#' Calculate \code{exp(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.exp <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(exp(x)),
    grads = list(x = quote(exp.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("exp.grad", function(x, grad)
{
  grad * exp(x)
})

#' Exponential Function
#'
#' Calculate \code{exp(1)^x}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
exp.cg.node <- function(x)
{
  cgraph::cg.exp(x)
}

#' Natual Logarithmic Function
#'
#' Calculate \code{log(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.ln <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(log(x)),
    grads = list(x = quote(ln.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("ln.grad", function(x, grad)
{
  grad / x
})

#' Logarithmic Base 2 Function
#'
#' Calculate \code{log2(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.log2 <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(log2(x)),
    grads = list(x = quote(log2.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("log2.grad", function(x, grad)
{
  grad / (x * log(2))
})

#' Logarithmic Base 2 Function
#'
#' Calculate \code{log2(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
log2.cg.node <- function(x)
{
  cgraph::cg.log2(x)
}

#' Logarithmic Base 10 Function
#'
#' Calculate \code{log10(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.log10 <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(log10(x)),
    grads = list(x = quote(log10.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("log10.grad", function(x, grad)
{
  grad / (x * log(10))
})

#' Logarithmic Base 10 Function
#'
#' Calculate \code{log10(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
log10.cg.node <- function(x)
{
  cgraph::cg.log10(x)
}

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#' @param name character scalar, name of the operation (optional).
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
cg.abs <- function(x, name = cgraph::name())
{
  cgraph::opr(name = name,
    call = quote(abs(x)),
    grads = list(x = quote(abs.grad(x, grad))),
    binding = list(x = x)
  )
}

# Export gradient
export("abs.grad", function(x, grad)
{
  grad * (x / abs(x))
})

#' Absolute Value
#'
#' Calculate \code{abs(x)}.
#'
#' @param x cg.node, placeholder for a numeric vector or array.
#'
#' @return cg.node, node of the operation.
#'
#' @author Ron Triepels
abs.cg.node <- function(x)
{
  cgraph::cg.abs(x)
}
