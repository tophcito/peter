\name{randomRoute}
\alias{randomRoute}
\title{find a random route from a to b in a given graph}
\usage{
  randomRoute(a, b, gr, maxiter = 1000)
}
\arguments{
  \item{a,b}{vertix id for start and end point of the
  route}

  \item{gr}{\code{igraph}}

  \item{maxiter}{The maximum number of nodes to be visited.
  The target must be reached within that limit, otherwise a
  warning will be issued. Defaults to 1000.}
}
\value{
  A named list with the elements \code{v} containing the
  vertices and \code{e} the edges visited during the
  journey.
}
\description{
  This function uses a very simple heuristic to get from
  \code{a} to \code{b} on a graph, returning the vertices
  and edges passed during the journey. For a description of
  the heuristic, see Details.
}
\details{
  The heuristic starts from \code{a}, and takes a random
  neighboring node. The selection probability depends on
  whether that node has already been visited. This search
  continues until the destination \code{b} or
  \code{maxiter} has been reached.
}
\examples{
data(vie)
rt1 <- randomRoute(a=43, b=findV("Skodagasse", vie), gr=attr(vie, "graph"))
}

