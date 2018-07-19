#' Block Summation
#'
#' Divide a vector or array in consecutive blocks of \code{n} elements and sum the elements at each position in these blocks.
#'
#' @param x, numeric vector or array, the object that is summed.
#' @param n, numeric scalar, block size. Defaults to 1.
#'
#' @note If \code{x} is an array and \code{n} is equal to the size of \code{x}'s first dimension, then \link[cgraph]{bsum} behaves as \link[base]{rowSums}.
#'
#' @return numeric vector, a \code{n}-dimensional vector, where the 1th element of the vector is the sum of each 1th element of the blocks, the 2nd element of the vector is the sum of each 2nd element of the blocks, and so on.
#'
#' @author Ron Triepels
bsum <- function(x, n = 1)
{
  n <- as.integer(n)

  .Call("bsum", x, n, PACKAGE = "cgraph")
}
