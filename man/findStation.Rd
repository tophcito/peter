\name{findStation}
\alias{findStation}
\title{Convert between StationID and Station Name}
\usage{
  findStation(station, ntw, ret = c("ID", "Name"))
}
\arguments{
  \item{station}{either a \code{character} vector or a
  \code{numeric} vector of length 1, with the station name
  or StationID respectively.}

  \item{ntw}{a class \code{ntw} public transport network
  object}

  \item{ret}{a \code{character} string detailing the
  direction of the conversion. If \code{ID}, the StationID,
  if \code{Name}, the station name will be returned. First
  letters are sufficient for this argument.}
}
\value{
  Either the \code{numeric} StationID or the
  \code{character} name of the station will be returned.
}
\description{
  This function takes either a StationID or a station name
  and converts between them.
}

