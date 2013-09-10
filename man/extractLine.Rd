\name{extractLine}
\alias{extractLine}
\title{Extract all Stations/Platforms from Network Line}
\usage{
  extractLine(ntw, line, ret.p = TRUE, ret.s = TRUE,
    ret.n = TRUE)
}
\arguments{
  \item{ntw}{class \code{ntw} public transport network
  object}

  \item{line}{a line specification either by key (as
  \code{numeric}) or name (as \code{character})}

  \item{ret.p,ret.s,ret.n}{if \code{TRUE} return the
  PlatformID, StationID or station name, respectively.}
}
\value{
  A data frame with (at most) PlatformID, StationID and
  station name for each station served by a line.
}
\description{
  This function returns all stations or platforms that a
  served by a line.
}

