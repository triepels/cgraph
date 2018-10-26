cg.integer <- function(length = cgraph::const(0), name)
{
  cgraph::opr(name = name,
    call = quote(integer),
    grads = list(
      quote(integer.grad)
    ),
    args = list(length)
  )
}

integer.grad <- function(length, val, grad)
{
  stop("cannot differentiate argument 'length' of operator 'cg.integer")
}

cg.is.integer <- function(x, name)
{
  cgraph::opr(name = name,
    call = quote(is.integer),
    grads = list(
      quote(is.integer.grad)
    ),
    args = list(x)
  )
}

is.integer.grad <- function(x, val, grad)
{
  stop("error")
}
