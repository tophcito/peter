\name{findRoute}
\alias{findRoute}
\title{Find the shortest route between A and B on a given network graph}
\usage{
  findRoute(a, b, ntw)
}
\arguments{
  \item{a,b}{start and end points of the journey,
  repsectively}

  \item{ntw}{a \code{ntw} public transit network with an
  attached graph.}
}
\value{
  A named list with three elements: the indices of the
  nodes that need to be traversed (\code{Vertices}), the
  indices of the edges connecting these nodes
  (\code{Edges}), and the accumulated cost (\code{Weight})
  during the travel.
}
\description{
  This function finds the shortest route between two points
  on a network. The \code{ntw} class public transit network
  objects needs to have a network graph attached to it. See
  \code{\link{makeGraph}} and \code{\link{addGraph}}.
}
\examples{
data(vie)
rt <- findRoute(a=findV("Ottakring", vie), b=findV("Skodagasse", vie), vie)
findS(rt$Vertices, vie)
}

