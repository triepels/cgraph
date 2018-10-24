init.grad <- function(x, index = 1)
{
  UseMethod("init.grad")
}

init.grad.default <- function(x, index = 1)
{
  stop(sprintf("unable to differentiate object (%s)", typeof(x)))
}

init.grad.numeric <- function(x, index = 1)
{
  grad <- x * 0

  grad[index] <- 1

  grad
}

add.grad <- function(grad, x, y)
{
  UseMethod("add.grad")
}

add.grad.default <- function(grad, x, y)
{
  stop(sprintf("unable to differentiate object (%s)", typeof(grad)))
}

add.grad.numeric <- function(grad, x, y)
{
  if(is.array(x))
  {
    grad
  }
  else
  {
    bsum(grad, length(x))
  }
}
