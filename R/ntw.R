### ntw class utility functions

##' Tests if an object is of class \code{ntw} or converts to that class.
##' 
##' Queries the class of a passed object and tests if it is of class \code{ntw},
##' representing a public transport network or coercing a list to be of that
##' class.
##' 
##' The coercion takes a list as obtained by \code{\link{getNetwork.vie}} and turns it
##' into a class \code{ntw} object. 
##' @title Network object test and coercion
##' @rdname is.ntw
##' @param x object to be tested
##' @return Returns \code{TRUE} if the tested object is a traffic network.
##' @export
##' @S3method is ntw
##' @author Christoph Waldhauser
is.ntw <- function(x) inherits(x, "ntw")


##' 
##' @param ntw a network specification, typically a list containing three 
##'   elements: stations, lines, platforms.
##' @param toASCII if \code{TRUE}, station and city names will be converted to 
##'   ASCII.
##' @param key a optional API key, to store together with the network meta data.
##'   See \code{\link{setAPIkey}}.
##' @return A class \code{ntw} object.
##' @rdname is.ntw
##' @export
as.ntw <- function(ntw, toASCII=FALSE, key=NULL) {
  res <- merge(x=merge(x=ntw$platforms,
                       y=ntw$stations,
                       by.x="FK_HALTESTELLEN_ID",
                       by.y="HALTESTELLEN_ID",
                       suffixes=c(".p", ".s")),
               y=ntw$lines,
               by.x="FK_LINIEN_ID",
               by.y="LINIEN_ID",
               suffixes=c(".p", ".l"))[,c(1:5, 9, 10, 14, 15, 16, 17, 18, 20, 21, 22, 23)]
  colnames(res) <- c("LineID", "StationID", "PlatformID", "Direction", 
                     "Order.p", "Lat.p", "Lon.p", "Name.s", 
                     "City", "CityID", "Lat.s", "Lon.s", 
                     "Name.l", "Order.l", "RT", "Type")
  ## fix faulty values: Station 214754157 Karl-Bednarik-Gasse has wrong GPS coordinates. Removing all points that are too far away from the other ones.
  errs.idx <- res[,"Lat.p"]<40
  res[errs.idx,c("Lon.s", "Lat.s", "Lon.p", "Lat.p")] <- NA
  name.stations <- as.character(res[,"Name.s"])
  name.cities <- as.character(res[,"City"])
  if (toASCII) {
    name.stations <- iconv(name.stations, from="", to="ascii//TRANSLIT")
    name.cities <- iconv(name.cities, from="", to="ascii//TRANSLIT")
  }
  res[,"Name.s"] <- name.stations
  res[,"City"] <- name.cities
  attr(res, "key") <- NULL
  class(res) <- c("ntw", "data.frame")
  return(res)
}

##' Summarize public transit network
##' 
##' This function summarizes a public transit network of class \code{ntw}.
##' @param object the \code{ntw} network object to be summarized
##' @param ... arguments passed to other methods. Currently ignored.
##' @return A named list with the number of lines, stations, platforms, the
##'   transport types offered, the extent of the network coverage in terms of
##'   longitude and latitude and the API key associated with it.
##' @S3method summary ntw
##' @method summary ntw
summary.ntw <- function(object, ...) {
  numLines <- length(unique(object$LineID))
  numStation <- length(unique(object$StationID))
  numPlatform <- nrow(object)
  transportTypes <- levels(object$Type)
  rng.lon <- range(c(object$Lon.s, object$Lon.p), na.rm=TRUE)
  rng.lat <- range(c(object$Lat.s, object$Lat.p), na.rm=TRUE)
  api.key <- attr(object, "key")
  res <- list(numLines=numLines,
              numStation=numStation,
              numPlatform=numPlatform,
              transportTypes=transportTypes,
              rng.lon=rng.lon,
              rng.lat=rng.lat,
              api.key=api.key)
  class(res) <- c("summary.ntw")
  return(res)
}

##' @param x summary of object to be printed
##' @return Invisibly returns the summary object.
##' @S3method print summary.ntw
##' @rdname summary.ntw
##' @method print summary.ntw
print.summary.ntw <- function(x, ...) {
  message("Public Transit Network")
  message(paste("Number of Lines:", x$numLines, 
                "Number of Stations:", x$numStation, 
                "Number of Platforms:", x$numPlatform))
  message("Transport types offered:")
  message(paste(c(" ", x$transportTypes), collapse=" "))
  message("Geographical coverage:")
  message(paste("  Longitude: From", x$rng.lon[1], "to", x$rng.lon[2]))
  message(paste("  Lattitude: From", x$rng.lat[1], "to", x$rng.lat[2]))
  message(paste("API Key:", x$api.key))
  invisible(x)  
}

##' Simple network visualization
##' 
##' This creates a simple representation of a public transit network. Each 
##' station in the network is represented by a dot in its geocoded location. The
##' color of the dot varies according to transport type.
##' @param x the \code{ntw} object to be plotted
##' @param y currently ignored
##' @param ... currently ignored
##' @param wrap if \code{TRUE}, the default, will seperate plots according to
##'   transport type.
##' @S3method plot ntw
##' @method plot ntw
plot.ntw <- function(x, y, ..., wrap=TRUE) {
  pl <- ggplot(data=x, aes(x=Lon.s, y=Lat.s, colour=Type)) + 
    geom_point() + labs(x="Longitude", y="Latitude")
  if (wrap) pl <- pl + facet_wrap(facets=~Type, scales="free")
  print(pl)
}

##' Set and test for API key
##' 
##' Most public transport data queries require an API key. These functions set
##' such a key and test for it.
##' @param ntw class \code{ntw} public transport network object
##' @param key the API key
##' @return For \code{setAPIkey}, the \code{ntw} object, with the API key added.
##' @export
setAPIkey <- function(ntw, key) {
  attr(ntw, "key") <- key
  return(ntw)
}

##' @rdname setAPIkey
##' @return For \code{hasAPIkey}, \code{TRUE} if an API key is defined for a
##'   network, \code{FALSE} otherwise.
hasAPIkey <- function(ntw) {
  return(!is.null(attr(ntw, "key")))
}