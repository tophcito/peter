\name{summary.ntw}
\alias{print.summary.ntw}
\alias{summary.ntw}
\title{Summarize public transit network}
\usage{
  \method{summary}{ntw} (object, ...)

  \method{print}{summary.ntw} (x, ...)
}
\arguments{
  \item{object}{the \code{ntw} network object to be
  summarized}

  \item{...}{arguments passed to other methods. Currently
  ignored.}

  \item{x}{summary of object to be printed}
}
\value{
  A named list with the number of lines, stations,
  platforms, the transport types offered, the extent of the
  network coverage in terms of longitude and latitude and
  the API key associated with it.

  Invisibly returns the summary object.
}
\description{
  This function summarizes a public transit network of
  class \code{ntw}.
}
\examples{
data(vie)
  summary(vie)
}

