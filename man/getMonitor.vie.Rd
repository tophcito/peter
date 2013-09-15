\name{getMonitor.vie}
\alias{getMonitor.vie}
\title{Retrieve departure real-time information from Vienna's Wiener Linien}
\usage{
  getMonitor.vie(rbl, ntw)
}
\arguments{
  \item{rbl}{the RBL code of the station. See
  \code{\link{findRBL}} for converting stations to RBL
  codes.}

  \item{ntw}{a class \code{ntw} public transit network
  object.}
}
\value{
  The list version of the returned JSON document.
}
\description{
  Obtaining real-time information from stations.
}
\details{
  Every station in the Wiener Linien network has (at least
  one) unique RBL code. These codes relate to physical
  monitors at the station. Here, these monitors can be
  queried for real time information.

  The object returned by the API is a complex JSON document
  that is automatically converted into an R \code{list}.
  For parsing that document, see
  \code{\link{parseMonitor.vie}}. \strong{Note:} accessing
  the API requires a key. See \code{\link{setAPIkey}} for
  how to set such a key.
}

