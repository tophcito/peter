##' Extract all Stations/Platforms from Network Line
##' 
##' This function returns all stations or platforms that a served by a line.
##' @param ntw class \code{ntw} public transport network object
##' @param line a line specification either by key (as \code{numeric}) or name 
##'   (as \code{character})
##' @param ret.p,ret.s,ret.n if \code{TRUE} return the PlatformID, StationID or 
##'   station name, respectively.
##' @return A data frame with (at most) PlatformID, StationID and station name
##'   for each station served by a line.
##' @export
extractLine <- function(ntw, line, ret.p=TRUE, ret.s=TRUE, ret.n=TRUE) {
  ## extract all stations/platforms related to a certain line
  l.n <- findLine(line, ntw, ret="ID")
  which <- c("StationID", "PlatformID", "Name.s")[c(ret.s, ret.p, ret.n)]
  if (any(c(ret.s, ret.p, ret.n))) res <- ntw[ntw[,"LineID"]==l.n,which]
  return(res)
}


##' Convert between LineID and Line Name
##' 
##' This function takes either a LineID or a line name and converts between 
##' them.
##' @param line either a \code{character} vector or a \code{numeric} vector of 
##'   length 1, with the line name or LineID respectively.
##' @param ntw a class \code{ntw} public transport network object
##' @param ret a \code{character} string detailing the direction of the
##'   conversion. If \code{ID}, the LineID, if \code{Name}, the line name will
##'   be returned. First letters are sufficient for this argument.
##' @return Either the \code{numeric} LineID or the \code{character} name of the
##'   line will be returned.
##' @export
findLine <- function(line, ntw, ret=c("ID", "Name")) {
  ret <- match.arg(ret)
  l.n <- suppressWarnings(as.numeric(line))
  if (is.na(l.n)) {
    res <- ntw[ntw[,"Name.l"]==line,"LineID"][[1]]
  } else if(l.n<100) {
    res <- ntw[ntw[,"Name.l"]==line,"LineID"][[1]]
  } else {
    res <- l.n
  }
  if (ret=="ID") res <- res
  if (ret=="Name") res <- as.character(ntw[ntw[,"LineID"]==res,"Name.l"][[1]])
  return(res)
}