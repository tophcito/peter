\name{addGraph}
\alias{addGraph}
\title{Add network graph to ntw}
\usage{
  addGraph(ntw, gr)
}
\arguments{
  \item{ntw}{a \code{ntw} public transit network object}

  \item{gr}{a \code{igraph} as produced by
  \code{link{makeGraph}}}
}
\value{
  A \code{ntw} object with an \code{igraph} as an attribute
  called \code{graph}.
}
\description{
  Adds a network graph (see \code{\link{igraph-package}})
  to a \code{ntw} class public transit network object.
}
\details{
  The igraph is stored as an attribute to the \code{ntw}
  object.
}
\examples{
data(vie)
  vie2 <- addGraph(vie, makeGraph(vie))
}

