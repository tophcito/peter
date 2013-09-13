##' Extract all lines serving a Station
##' 
##' This function returns all lines that serve a station.
##' @param ntw class \code{ntw} public transport network object
##' @param station a station specification either by key (as \code{numeric}) or 
##'   name (as \code{character})
##' @param ret.l,ret.n if \code{TRUE} return the LineID or line name, 
##'   respectively.
##' @return A data frame with (at most) LineID and line name serving that
##'   station.
##' @export
extractStation <- function(ntw, station, ret.l=TRUE, ret.n=TRUE) {
  s.n <- findStation(station, ntw, ret="ID")
  which <- c("LineID", "Name.l")[c(ret.l, ret.n)]
  if (any(c(ret.l, ret.n))) res <- unique(ntw[ntw[,"StationID"]==s.n,which])
  return(res)
}


##' Convert between StationID and Station Name
##' 
##' This function takes either a StationID or a station name and converts between 
##' them.
##' @param station either a \code{character} vector or a \code{numeric} vector of 
##'   length 1, with the station name or StationID respectively.
##' @param ntw a class \code{ntw} public transport network object
##' @param ret a \code{character} string detailing the direction of the
##'   conversion. If \code{ID}, the StationID, if \code{Name}, the station name will
##'   be returned. First letters are sufficient for this argument.
##' @return Either the \code{numeric} StationID or the \code{character} name of the
##'   station will be returned.
##' @export
findStation <- function(station, ntw, ret=c("ID", "Name")) {
  ret <- match.arg(ret)
  s.n <- suppressWarnings(as.numeric(station))
  if (is.na(s.n)) {
    res <- ntw[ntw[,"Name.s"]==station,"StationID"][[1]]
  } else if(s.n<100) {
    res <- ntw[ntw[,"Name.s"]==station,"StationID"][[1]]
  } else {
    res <- s.n
  }
  if (ret=="ID") res <- res
  if (ret=="Name") res <- as.character(ntw[ntw[,"StationID"]==res,"Name.s"][[1]])
  return(res)
}

##' Station and Line's RBL Code
##' 
##' RBL codes identify locations that vehicles reach. They are unique for any 
##' given station, line and direction. This function looks it up in the meta
##' data table.
##' @param station \code{numeric} StationID
##' @param line \code{numeric} LineID
##' @param direction either \code{R} or \code{H}
##' @param ntw a class \code{ntw} public transport network
##' @return The RBL code for the station, line, and direction specification.
##' @export
findRBL <- function(station, line, direction=c("H", "R"), ntw) {
  direction <- match.arg(direction)
  res <- ntw[ntw[,"StationID"]==station & ntw[,"LineID"]==line & ntw[,"Direction"]==direction, "RBL"]
  return(res)
}
