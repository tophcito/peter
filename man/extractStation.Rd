\name{extractStation}
\alias{extractStation}
\title{Extract all lines serving a Station}
\usage{
  extractStation(ntw, station, ret.l = TRUE, ret.n = TRUE)
}
\arguments{
  \item{ntw}{class \code{ntw} public transport network
  object}

  \item{station}{a station specification either by key (as
  \code{numeric}) or name (as \code{character})}

  \item{ret.l,ret.n}{if \code{TRUE} return the LineID or
  line name, respectively.}
}
\value{
  A data frame with (at most) LineID and line name serving
  that station.
}
\description{
  This function returns all lines that serve a station.
}

