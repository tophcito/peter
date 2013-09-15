\name{setEdgeWeights}
\alias{setEdgeWeights}
\title{Weighting a graph with waiting times}
\usage{
  setEdgeWeights(ntw, gr, w)
}
\arguments{
  \item{ntw,gr}{a class \code{ntw} public transit network
  object or an igraph.  Either one needs to be specified,
  if both a specified, \code{gr} is ignored.}

  \item{w}{optional weights to be used on the edges (in the
  order of edges in the igraph).}
}
\description{
  This function adds edge weights (read waiting times) to
  the edges in a public transit network graph. If no
  weights are specified at calling time, weights are
  created at random, following a poission distribution with
  \eqn{\lambda = 5}{lambda=5}.
}
\examples{
data(vie)
vie.w <- setEdgeWeights(vie)
}

