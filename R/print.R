print.cgraph <- function(x, ...)
{
  cat("<cgraph>")
}

print.cg.node = function(x, ...)
{
  cat(sprintf("<cg.node: %s> '%s'", .Call("cg_types")[attr(x, "type") + 1], x))
}

print.cg.environment <- function(x, ...)
{
  cat("<cg.environment>")
}
