# Function definition
delayedAssign("[", cg_function(
  def = base::`[`,
  grads = list(
    function(x, ..., drop = FALSE, val, grad)
    {
      if(is.array(x))
      {
        out <- array(0, dim(x))
        out[...] <- grad
        out
      }
      else
      {
        out <- rep(0, length(x))
        out[...] <- grad
        out
      }
    }
  )
))

#' @export
`[.cg_node` <- function(x, ..., drop = FALSE)
{
  if(!drop)
  {
    cg_operator(`[`, c(x, dots()))
  }
  else
  {
    cg_operator(`[`, c(x, dots(), drop = drop))
  }
}
