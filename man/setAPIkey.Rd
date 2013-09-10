\name{setAPIkey}
\alias{hasAPIkey}
\alias{setAPIkey}
\title{Set and test for API key}
\usage{
  setAPIkey(ntw, key)

  hasAPIkey(ntw)
}
\arguments{
  \item{ntw}{class \code{ntw} public transport network
  object}

  \item{key}{the API key}
}
\value{
  For \code{setAPIkey}, the \code{ntw} object, with the API
  key added.

  For \code{hasAPIkey}, \code{TRUE} if an API key is
  defined for a network, \code{FALSE} otherwise.
}
\description{
  Most public transport data queries require an API key.
  These functions set such a key and test for it.
}

