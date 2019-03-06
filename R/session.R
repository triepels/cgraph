#' Get Active Graph
#'
#' Get the graph that is currently active.
#'
#' @return cg_graph object, the graph that is currently active.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Retrieve the graph from the session
#' cg_session_graph()
#'
#' @author Ron Triepels
#' @export
cg_session_graph <- function()
{
  .Call("cg_session_graph", PACKAGE = "cgraph")
}

#' Change Active Graph
#'
#' Set a graph to be the active graph.
#'
#' @param graph cg_graph object, the graph that is activated.
#'
#' @note Any nodes that are created are automatically added to the active graph. This also applies to operations that are created by overloaded S3 functions that do not follow the cg_<name> naming convention (such as primitive inflix functions '+' and '-').
#'
#' Only one graph can be active at a time. The active graph can be changed by calling this function on another cg_graph object.
#'
#' @return none.
#'
#' @examples # Initialize a computational graph
#' x <- cg_graph()
#'
#' # Initialize another computational graph. It becomes the active graph.
#' y <- cg_graph()
#'
#' # Set x to be the active graph
#' cg_session_set_graph(y)
#'
#' @author Ron Triepels
#' @export
cg_session_set_graph <- function(graph)
{
  .Call("cg_session_set_graph", graph, PACKAGE = "cgraph")
}

#' @export
print.cg_session <- function(x, ...)
{
  cat("<cg_session>")
}
