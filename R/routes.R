### Finding routes from one station to another

##' Find the shortest route between A and B on a given network graph
##' 
##' This function finds the shortest route between two points on a network. The 
##' \code{ntw} class public transit network objects needs to have a network
##' graph attached to it. See \code{\link{makeGraph}} and
##' \code{\link{addGraph}}.
##' @param a,b start and end points of the journey, repsectively
##' @param ntw a \code{ntw} public transit network with an attached graph.
##' @return the index of the nodes that need to be traversed to go from \code{a}
##'   to \code{b}.
##' @export
findRoute <- function(a, b, ntw) {
  ntw.gr <- attr(ntw, "graph")
  if (is.null(ntw.gr)) stop("Network has no graph attached. Use addGraph first")
  res <- unlist(get.shortest.paths(ntw.gr, from=a, to=b))
  return(res)
}
