### Finding routes from one station to another

##' Find the shortest route between A and B on a given network graph
##' 
##' This function finds the shortest route between two points on a network. The 
##' \code{ntw} class public transit network objects needs to have a network 
##' graph attached to it. See \code{\link{makeGraph}} and 
##' \code{\link{addGraph}}.
##' @param a,b start and end points of the journey, repsectively
##' @param ntw a \code{ntw} public transit network with an attached graph.
##' @return A named list with three elements: the indices of the nodes that need
##'   to be traversed (\code{Vertices}), the indices of the edges connecting 
##'   these nodes (\code{Edges}), and the accumulated cost (\code{Weight})
##'   during the travel.
##' @export
##' @examples
##' data(vie)
##' rt <- findRoute(a=findV("Ottakring", vie), b=findV("Skodagasse", vie), vie)
##' findS(rt$Vertices, vie)
findRoute <- function(a, b, ntw) {
  ntw.gr <- attr(ntw, "graph")
  if (is.null(ntw.gr)) stop("Network has no graph attached. Use addGraph first")
  res <- get.shortest.paths(ntw.gr, from=a, to=b, output="both")
  resE <- res$epath[[1]]
  resV <- res$vpath[[1]]
  resW <- sum(E(ntw.gr)$weight[resE])
  res <- list(Vertices=resV, Edges=resE, Weight=resW)
  return(res)
}
