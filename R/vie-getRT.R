### Functions to retrieve Vienna's Wiener Linien real time data

##' Retrieve departure real-time information from Vienna's Wiener Linien
##' 
##' Obtaining real-time information from stations.
##' 
##' Every station in the Wiener Linien network has (at least one) unique RBL 
##' code. These codes relate to physical monitors at the station. Here, these 
##' monitors can be queried for real time information.
##' 
##' The object returned by the API is a complex JSON document that is 
##' automatically converted into an R \code{list}. For parsing that document, 
##' see \code{\link{parseMonitor.vie}}. \strong{Note:} accessing the API 
##' requires a key. See \code{\link{setAPIkey}} for how to set such a key.
##' @param rbl the RBL code of the station. See \code{\link{findRBL}} for
##'   converting stations to RBL codes.
##' @param ntw a class \code{ntw} public transit network object.
##' @return The list version of the returned JSON document.
##' @export
getMonitor.vie <- function(rbl, ntw) {
  api.key <- attr(ntw, "key")
  if (is.null(api.key)) stop("No API key. Use setAPIkey() to set key.")
  query.base <- "http://www.wienerlinien.at/ogd_realtime/monitor?"
  query.rbl <- paste0("rbl=", rbl)
  query.rbl <- paste(query.rbl, collapse="&")
  query.key <- paste0("sender=", attr(ntw, "key"))
  res <- fromJSON(getURL(paste(query.base, query.rbl, query.key, sep="&")))
  return(res)
}

##' Extracting timing information from API answer
##' 
##' The api answer returned is quite complex. This function extracts the planned
##' and currently expected departure times for vehicles scheduled to arrive in 
##' the next 70 minutes along the class (barrier free, or not) of the vehicle.
##' 
##' As multiple lines might be covered in a single answer, optionally, a line 
##' can be singled out here.
##' @param mon an API answer, as returned by \code{\link{getMonitor.vie}}
##' @param line a line identifier
##' @return a data frame with planned, current time and barrier free status for
##'   each line in the API answer.
##' @export
##' @examples
##' \dontrun{
##' mon <- getMonitor.vie(rbl="141", ntw=vie)
##' parseMonitor.vie(mon)
##' }
parseMonitor.vie <- function(mon, line) {
  dta <- mon$data[[1]]
  lns <- lapply(dta, function(x) x[["lines"]][[1]])
  lid <- sapply(lns, function(x) x[["lineId"]])
  if (!missing(line)) {
    line.idx <- lid==line
  } else {
    line.idx <- rep(TRUE, time=length(lid))
  }
  bf <- sapply(lns, function(x) x[["barrierFree"]])[line.idx]
  dep <- lapply(lns, function(x) x[["departures"]])[line.idx]
  dep.c <- sapply(dep, function(x) lapply(x$departure, function(y) y$departureTime$countdown))
  dep.r <- sapply(dep, function(x) lapply(x$departure, function(y) y$departureTime$timeReal))
  bf2 <- lapply(dep, function(x) lapply(x$departure, function(y) y$vehicle$barrierFree))
  bf2 <- sapply(bf2, function(x) {
    this.val <- unlist(x)[1]
    this.res <- sapply(x, function(y) {
      if (is.null(y)) {
        y <- !this.val
      } else {
        y
      }
    })
  })
  res <- data.frame(dep.c, dep.r, bf2)
  colnames(res) <- paste(rep(c("cnt", "act", "bf"), each=sum(line.idx)), 
                         rep(lid[line.idx], times=3), sep=".")
  return(res)
}

