\name{getNetwork.vie}
\alias{getNetwork.vie}
\title{Download Vienna's Wiener Linien network data}
\usage{
  getNetwork.vie(kind = c("stations", "lines", "platforms"))
}
\arguments{
  \item{kind}{a vector specifying the data sets to be
  downloaded. Defaults to all available data. See Details.}
}
\value{
  A named list with (at most) three elements named
  \code{stations}, \code{lines}, and \code{platforms}.
}
\description{
  This function provides access to Vienna's Wiener Linien
  network meta data. Data on stations, lines, and platforms
  are retrieved from online sources.
}
\details{
  A networks meta data is required to produce a model of
  the graph representing the network. Wiener Linien
  provides csv files for stations, lines and platforms. The
  following sections describe the data content. Usually,
  all three data sets are required to obtain a meaningful
  representation of the network graph.
}
\section{License}{
  Wiener Linien and the Vienna city government provides
  these data under a
  \href{http://creativecommons.org/licenses/by/3.0/at/deed.de}{Creative
  Commons Attribution license}. The attribution clause is
  fulfilled by stating the data source: Stadt Wien -
  data.wien.gv.at. The authoritative description of the
  data set can be found in the
  \href{https://open.wien.at/site/datensatz/?id=add66f20-d033-4eee-b9a0-47019828e698}{original
  data set description}.
}

\section{Stations}{
  Stations group together platforms. Some real-time data is
  available for / applies to stations instead of platforms.
  Essentially, stations are the nodes in a coarse network
  model. \tabular{ll}{ HALTESTELLEN_ID \tab Station ID
  (key)\cr TYP \tab Object type \cr DIVA \tab Internal
  numeric code \cr NAME \tab Station name \cr GEMEINDE \tab
  Name of municipiality \cr GEMEINDE_ID \tab municipiality
  ID (key) \cr WGS84_LAT \tab Latitude of station location
  \cr WGS84_LON \tab Longitude of station location \cr
  STAND \tab Date of data export}
}

\section{Lines}{
  Lines are the edges connecting the network nodes: they
  link up platforms and in that stations.

  \tabular{ll}{ LINIEN_ID \tab Line ID (key) \cr
  BEZEICHNUNG \tab Line name \cr REIHENFOLGE \tab Internal
  line order \cr ECHTZEIT \tab Availability of real-time
  data \cr VERKEHRSMITTEL \tab Kind of transport \cr STAND
  \tab Date of data export}
}

\section{Platforms}{
  Platform data describes nodes in a much finer resolution
  than \code{stations}. Essentially, this table links
  \code{lines} and \code{stations} together.

  \tabular{ll}{ Steig_ID \tab Platform ID (key) \cr
  FK_LINIEN_ID \tab Line ID (key) \cr FK_HALTESTELLEN_ID
  \tab Station ID (key) \cr RICHTUNG \tab Direction of
  ordering \cr REIHENFOLGE \tab Ordering withing
  direction\cr RBL_NUMMER \tab Internal numeric code \cr
  BEREICH \tab Platform area \cr STEIG \tab Name of
  Platform (internal) \cr STEIG_WGS84_LAT \tab Latitude of
  platform location \cr STEIG_WGS84_LON \tab Longitude of
  platform location \cr STAND \tab Date of data export}
}
\examples{
\dontrun{
  vie.ntw <- getNetwork.vie() # get all network data
  }
}

