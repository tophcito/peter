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
  lines <- lapply(lines, extractLine, ntw=ntw)
  edges <- lapply(lines, function(this.line) {
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
  res <- NULL
  for (i in 1:length(edges)) {
    res <- rbind(res, edges[[i]])
  }
  res <- graph.edgelist(res)
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