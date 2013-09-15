\name{parseMonitor.vie}
\alias{parseMonitor.vie}
\title{Extracting timing information from API answer}
\usage{
  parseMonitor.vie(mon, line)
}
\arguments{
  \item{mon}{an API answer, as returned by
  \code{\link{getMonitor.vie}}}

  \item{line}{a line identifier}
}
\value{
  a data frame with planned, current time and barrier free
  status for each line in the API answer.
}
\description{
  The api answer returned is quite complex. This function
  extracts the planned and currently expected departure
  times for vehicles scheduled to arrive in the next 70
  minutes along the class (barrier free, or not) of the
  vehicle.
}
\details{
  As multiple lines might be covered in a single answer,
  optionally, a line can be singled out here.
}
\examples{
\dontrun{
mon <- getMonitor.vie(rbl="141", ntw=vie)
parseMonitor.vie(mon)
}
}

