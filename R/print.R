print.cgraph <- function(x, ...)
{
  cat(sprintf("<cgraph: %s>", address(x)))
}

print.cg.node = function(x, ...)
{
  val <- NULL

  tryCatch(
  {
    if(is.null(session$graph))
    {
      stop("No current graph set")
    }

    val <- get(x, envir = session$graph$run(x))
  },
  error = function(e)
  {
    e$call <- NULL

    warning(e)
  })

  if(is.null(session$graph))
  {
    cat(sprintf("<cg.node: %s>\n", x))
  }
  else
  {
    if(!is.null(val))
    {
      print(val); cat("\n")
    }

    cat(sprintf("<cg.node: %s> @ %s\n", x, address(session$graph)))
  }
}
