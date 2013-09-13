\name{makeGraph}
\alias{makeGraph}
\title{Convert network object into igraph}
\usage{
  makeGraph(ntw, lines)
}
\arguments{
  \item{ntw}{a class \code{ntw} representation of a public
  transit network}

  \item{lines}{optional argument specifying the lines to be
  taken out of the network to form the graph.}
}
\value{
  A \code{\link{igraph}} graph object.
}
\description{
  This function turns a ntw object (or some of its lines)
  into a \code{\link{igraph}} graph representation.
}
\examples{
\dontrun{
## While graphs are typically not need externally, they can come in handy when developing.
## Here are some notes on how to use them
foo.gr <- makeGraph(vie, lines=list("13A", "U3"))
tkplot(foo.gr) #for an interactive tk based plot
## V() and E() can be used to access all nodes and edges:
V(foo.gr)[1:10]$name #The names of the first ten nodes
}
}

