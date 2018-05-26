print.cgraph <- function(x, ...)
{
  cat("<cgraph>")
}

print.cg.node = function(x, ...)
{
  val <- NULL

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

  cat(sprintf("<cg.node: %s> '%s'\n\n", .Call("cg_types")[attr(x, "type") + 1], x))

  print(val)
}

print.cg.environment <- function(x, ...)
{
  cat("<cg.environment>")
}
