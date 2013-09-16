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

##' find a random route from a to b in a given graph
##' 
##' This function uses a very simple heuristic to get from \code{a} to \code{b} 
##' on a graph, returning the vertices and edges passed during the journey. For 
##' a description of the heuristic, see Details.
##' 
##' The heuristic starts from \code{a}, and takes a random neighboring node. The
##' selection probability depends on whether that node has already been visited.
##' This search continues until the destination \code{b} or \code{maxiter} has 
##' been reached.
##' @param a,b vertix id for start and end point of the route
##' @param gr \code{igraph}
##' @param maxiter The maximum number of nodes to be visited. The target must be
##'   reached within that limit, otherwise a warning will be issued. Defaults to
##'   1000.
##' @return A named list with the elements \code{v} containing the vertices and
##'   \code{e} the edges visited during the journey.
##' @export
##' @examples
##' data(vie)
##' rt1 <- randomRoute(a=43, b=findV("Skodagasse", vie), gr=attr(vie, "graph"))
randomRoute <- function(a, b, gr, maxiter=1000) {
  actualV <- a
  vertices <- actualV
  edges <- NULL
  i <- 0
  while (actualV != b & i < maxiter) {
    this.n <- neighbors(graph=gr, v=actualV, mode="out")
    ## how often are they alreay contained within vertices
    this.f <- sapply(as.list(this.n), function(x) sum(vertices==x))
    this.p <- -1 * this.f + max(this.f)
    if (all(this.p==0)) this.p <- this.p + 1
    newV <- this.n[sample.int(length(this.n), 1, prob=this.p)]
    newE <- E(gr, P=c(actualV, newV))
    actualV <- newV
    edges <- c(edges, newE)
    vertices <- c(vertices, actualV)
    i <- i + 1
  }
  if (i==maxiter) warning("Search did not reach target!")
  res <- list(v=vertices, e=edges)
  return(res)
}