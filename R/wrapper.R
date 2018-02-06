#' Array Transposition
#'
#' Transpose the row and columns of an array while keeping its dimensions the same.
#'
#' @section Usage:
#' \preformatted{perm(a)}
#'
#' @section Agruments:
#' \describe{
#' \item{a}{array, the array to be transposed.}
#' }
#'
#' @note This is a wrapper function for \code{aperm(a, replace(1:length(dim(a)), 1:2, 2:1), resize = F)}.
#'
#' @return array, transposed array.
#'
#' @name perm
#' @author Ron Triepels
perm <- function(a)
{
  return(aperm(a, replace(1:length(dim(a)), 1:2, 2:1), resize = F))
}
