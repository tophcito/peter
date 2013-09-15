##' Convert network object into igraph
##' 
##' This function turns a ntw object (or some of its lines) into a 
##' \code{\link{igraph}} graph representation.
##' @param ntw a class \code{ntw} representation of a public transit network
##' @param lines optional argument specifying the lines to be taken out of the 
##'   network to form the graph.
##' @return A \code{\link{igraph}} graph object.
##' @export
##' @examples
##' \dontrun{
##' ## While graphs are typically not need externally, they can come in handy when developing.
##' ## Here are some notes on how to use them
##' foo.gr <- makeGraph(vie, lines=list("13A", "U3"))
##' tkplot(foo.gr) #for an interactive tk based plot
##' ## V() and E() can be used to access all nodes and edges:
##' V(foo.gr)[1:10]$name #The names of the first ten nodes
##' }
makeGraph <- function(ntw, lines) {
  if (missing(lines)) lines <- as.list(unique(as.character(vie[,"Name.l"])))
  lin <- lapply(lines, extractLine, ntw=ntw)
  names(lin) <- unlist(lines)
  edges <- lapply(lin, function(this.line) {
    l.o <- this.line[order(this.line[,"Direction"], this.line[,"Order.p"]),]
    l.os <- split(l.o, l.o["Direction"])
    ed.ll <- lapply(l.os, function(this.l.os){
      ed.l <- matrix(nrow=nrow(this.l.os)-1, ncol=2)
      for (i in 1:(nrow(this.l.os)-1)) {
        this.start <- this.l.os[i,"Name.s"]
        this.end <- this.l.os[(i+1),"Name.s"]
        ed.l[i,1] <- this.start
        ed.l[i,2] <- this.end
      }
      return(ed.l)
    })
    res <- NULL
    for (i in 1:length(ed.ll)) {
      res <- rbind(res, ed.ll[[i]])
    }
    return(res)
  })
  lineLookup <- NULL
  for (i in 1:length(lin)) {
    lineLookup <- rbind(lineLookup, c(nrow(lin[[i]])-2, names(lin)[i]))
    # nrow(lin[[i]])-2 to remove the last stop in each direction
  }
  lN <- unlist(apply(lineLookup, 1, function(x) rep(x[2], times=x[1])))
  res <- NULL 
  for (i in 1:length(edges)) {
    res <- rbind(res, edges[[i]])
  }
  res <- graph.edgelist(res)
  E(res)$lineName <- lN
  stat.names <- V(res)$name
  stat.locs <- lapply(as.list(stat.names), function(x) unique(ntw[ntw[,"Name.s"]==x, c("Lon.s", "Lat.s")]))
  res.locs <- NULL
  for (i in 1:length(stat.locs)) res.locs <- rbind(res.locs, stat.locs[[i]])
  res$layout <- as.matrix(res.locs)
  return(res)
}

##' Add network graph to ntw
##' 
##' Adds a network graph (see \code{\link{igraph-package}}) to a \code{ntw} 
##' class public transit network object.
##' 
##' The igraph is stored as an attribute to the \code{ntw} object.
##' @param ntw a \code{ntw} public transit network object
##' @param gr a \code{igraph} as produced by \code{link{makeGraph}}
##' @return A \code{ntw} object with an \code{igraph} as an attribute called
##'   \code{graph}.
##' @export
##' @examples
##'   data(vie)
##'   vie2 <- addGraph(vie, makeGraph(vie))
addGraph <- function(ntw, gr) {
  attr(ntw, "graph") <- gr
  return(ntw)
}

##' Weighting a graph with waiting times
##' 
##' This function adds edge weights (read waiting times) to the edges in a 
##' public transit network graph. If no weights are specified at calling time, 
##' weights are created at random, following a poission distribution with 
##' \eqn{\lambda = 5}{lambda=5}.
##' @param ntw,gr a class \code{ntw} public transit network object or an igraph.
##'   Either one needs to be specified, if both a specified, \code{gr} is 
##'   ignored.
##' @param w optional weights to be used on the edges (in the order of edges in
##'   the igraph).
##' @export
##' @examples
##' data(vie)
##' vie.w <- setEdgeWeights(vie)  
setEdgeWeights <- function(ntw, gr, w) {
  if(missing(ntw) & missing(gr)) stop("Specify either ntw or gr object.")
  if(!missing(ntw)) gr <- attr(ntw, "graph")
  if(is.null(gr)) stop("Network has no graph attached. Use addGraph first, 
                       or specify graph directly.")
  if(missing(w)) w <- rpois(ecount(gr), lambda=5)
  E(gr)$weight <- w
  if(!missing(ntw)) { 
    res <- addGraph(ntw=ntw, gr=gr)
  } else {
    res <- gr
  }
  return(res)
}
