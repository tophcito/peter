\name{plot.ntw}
\alias{plot.ntw}
\title{Simple network visualization}
\usage{
  \method{plot}{ntw} (x, y, ..., wrap = TRUE)
}
\arguments{
  \item{x}{the \code{ntw} object to be plotted}

  \item{y}{currently ignored}

  \item{...}{currently ignored}

  \item{wrap}{if \code{TRUE}, the default, will seperate
  plots according to transport type.}
}
\description{
  This creates a simple representation of a public transit
  network. Each station in the network is represented by a
  dot in its geocoded location. The color of the dot varies
  according to transport type.
}

