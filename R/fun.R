bsum <- function(x, n)
{
  n <- as.integer(n)

  .Call("bsum", x, n, PACKAGE = "cgraph")
}
