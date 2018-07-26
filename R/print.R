print.cgraph <- function(x, ...)
{
  cat(sprintf("<cgraph: %s>\n", address(x)))
}

print.cg.node = function(x, ..., autorun = getOption("cg.autorun"))
{
  val <- NULL

  if(is.null(autorun))
  {
    autorun = TRUE
  }

  if(autorun)
  {
    tryCatch(
    {
      val <- get(x, envir = run(x))
    },
    error = function(e)
    {
      e$call <- NULL

      warning(e)
    })
  }

  if(is.null(session$graph) | !autorun)
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
