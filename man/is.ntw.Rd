\name{is.ntw}
\alias{as.ntw}
\alias{is.ntw}
\title{Network object test and coercion}
\usage{
  is.ntw(x)

  as.ntw(ntw, toASCII = FALSE, key = NULL)
}
\arguments{
  \item{x}{object to be tested}

  \item{ntw}{a network specification, typically a list
  containing three elements: stations, lines, platforms.}

  \item{toASCII}{if \code{TRUE}, station and city names
  will be converted to ASCII.}

  \item{key}{a optional API key, to store together with the
  network meta data.  See \code{\link{setAPIkey}}.}
}
\value{
  Returns \code{TRUE} if the tested object is a traffic
  network.

  A class \code{ntw} object.
}
\description{
  Tests if an object is of class \code{ntw} or converts to
  that class.
}
\details{
  Queries the class of a passed object and tests if it is
  of class \code{ntw}, representing a public transport
  network or coercing a list to be of that class.

  The coercion takes a list as obtained by
  \code{\link{getNetwork.vie}} and turns it into a class
  \code{ntw} object.
}
\examples{
\dontrun{
  vie.lis <- getNetwork.vie()
  vie <- as.ntw(vie.lis)
  }
}
\author{
  Christoph Waldhauser
}

