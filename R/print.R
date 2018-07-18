print.cgraph <- function(x, ...)
{
  cat("<cgraph>")
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

  cat(sprintf("<cg.node: %s>\n\n", x))

  print(val)
}
