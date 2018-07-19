#' Block Summation
#'
#' Divide a vector or array in consecutive blocks of \code{n} elements and sum the elements at each position in these blocks.
#'
#' @param x, vector or array, the object that is summed.
#' @param n, integer scalar, block size. Defaults to 1.
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

#' Create Numeric Array
#'
#' Create an numeric array without recycling. Missing values are replaced with zeroes.
#'
#' @param x, numeric vector or array, the data to fill the array.
#' @param dim, integer vector, the dimensions of the array. Defaults to the length of \code{x}.
#' @param dimnames, list, names for the dimensions. Is ignored when \code{dimnames} is \code{NULL}. Defaults to \code{NULL}.
#'
#' @note In contrast to base function \link[base]{array}, this function only works for numerical inputs and does not recycle elements.
#'
#' @return numeric array, \code{x} coerced to an array with dimensions \code{dim}.
#'
#' @author Ron Triepels
array0 <- function(x, dim = length(x), dimnames = NULL)
{
  dim <- as.integer(dim)

  .Call("array0", x, dim, dimnames, PACKAGE = "cgraph")
}
