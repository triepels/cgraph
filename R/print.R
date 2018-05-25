print.cgraph <- function(x, ...)
{
  cat("<cgraph>")
}

print.cg.node = function(x, ...)
{
  val <- NULL

  if(!exists("graph", envir = .session))
  {
    stop("No current graph set")
  }

  tryCatch(
  {
    val <- get(x, envir = .session$graph$run(x))
  },
  error = function(e)
  {
    e$call <- NULL

    warning(e)
  })

  print(val)
}

print.cg.environment <- function(x, ...)
{
  cat("<cg.environment>")
}
