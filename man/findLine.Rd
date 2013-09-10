\name{findLine}
\alias{findLine}
\title{Convert between LineID and Line Name}
\usage{
  findLine(line, ntw, ret = c("ID", "Name"))
}
\arguments{
  \item{line}{either a \code{character} vector or a
  \code{numeric} vector of length 1, with the line name or
  LineID respectively.}

  \item{ntw}{a class \code{ntw} public transport network
  object}

  \item{ret}{a \code{character} string detailing the
  direction of the conversion. If \code{ID}, the LineID, if
  \code{Name}, the line name will be returned. First
  letters are sufficient for this argument.}
}
\value{
  Either the \code{numeric} LineID or the \code{character}
  name of the line will be returned.
}
\description{
  This function takes either a LineID or a line name and
  converts between them.
}

