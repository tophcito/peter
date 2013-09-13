\name{findRBL}
\alias{findRBL}
\title{Station and Line's RBL Code}
\usage{
  findRBL(station, line, direction = c("H", "R"), ntw)
}
\arguments{
  \item{station}{\code{numeric} StationID}

  \item{line}{\code{numeric} LineID}

  \item{direction}{either \code{R} or \code{H}}

  \item{ntw}{a class \code{ntw} public transport network}
}
\value{
  The RBL code for the station, line, and direction
  specification.
}
\description{
  RBL codes identify locations that vehicles reach. They
  are unique for any given station, line and direction.
  This function looks it up in the meta data table.
}

