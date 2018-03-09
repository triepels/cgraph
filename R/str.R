str.cgraph <- function(object, ...)
{
  max <- 100

  n.nodes <- length(object$nodes)

  cat(sprintf("<cgraph> : %d node(s)\n", n.nodes))

  if(n.nodes > 0)
  {
    for(node in object$nodes[1:min(n.nodes, max)])
    {
      cat(sprintf(" '%s': %s\n", node, .Call("cg_types")[attr(node, "type") + 1]))
    }

    if(n.nodes > max)
    {
      cat("  [list output truncated]")
    }
  }
}

str.cg.environment <- function(object, ...)
{
  max <- 100

  n.nodes <- length(object)

  cat(sprintf("<cg.environment> : %d object(s)\n", n.nodes))

  if(n.nodes > 0)
  {
    for(node in ls(object)[1:min(n.nodes, max)])
    {
      value = get(node, object);

      dim = ifelse(is.array(value), paste(dim(value), collapse = " x "), length(value))

      cat(sprintf(" %s: %s (%s)\n", node, typeof(value), dim))
    }

    if(n.nodes > max)
    {
      cat("  [list output truncated]")
    }
  }
}
