print.cgraph <- function(x, ...)
{
  cat("<cgraph>")
}

print.cg.node = function(x, ...)
{
  val <- NULL

  types <- c("constant", "input", "parameter", "expression")

  tryCatch(
  {
    if(!exists("graph", envir = .cg))
    {
      stop("No current graph set")
    }

    val <- get(x, envir = .cg$graph$run(x))
  },
  error = function(e)
  {
    e$call <- NULL

    warning(e)
  })

  cat(sprintf("<cg.node: %s> '%s'\n\n", types[attr(x, "type") + 1], x))

  print(val)
}
