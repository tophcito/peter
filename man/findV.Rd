\name{findV}
\alias{findS}
\alias{findV}
\title{Converting between station names and graphs' vertex IDs}
\usage{
  findV(station, ntw)

  findS(vid, ntw)
}
\arguments{
  \item{station}{the station name to look up}

  \item{ntw}{the class \code{ntw} public transit network
  object}

  \item{vid}{one or more numeric vertex id(s)}
}
\value{
  The vertex id as numeric or the station name(s).
}
\description{
  These functions take a station name or a vertex ID and
  convert between the two.
}
\examples{
data(vie)
findV("Neubaugasse", vie)
findS(142, vie)
}

