\name{extractLine}
\alias{extractLine}
\title{Extract all Stations/Platforms from Network Line}
\usage{
  extractLine(ntw, line, ret.p = TRUE, ret.s = TRUE, ret.r,
    ret.n = TRUE, ret.o = TRUE, ret.d = TRUE)
}
\arguments{
  \item{ntw}{class \code{ntw} public transport network
  object}

  \item{line}{a line specification either by key (as
  \code{numeric}) or name (as \code{character})}

  \item{ret.p,ret.s,ret.r,ret.n,ret.o,ret.d}{if \code{TRUE}
  return the PlatformID, StationID, RBL code, station name,
  order and direction respectively.}
}
\value{
  A data frame with (at most) PlatformID, StationID, RBL
  code, station name, ordering, and direction for each
  station served by a line.
}
\description{
  This function returns all stations or platforms that a
  served by a line.
}
\examples{
data(vie)
  extractLine(vie, "13A")
}

