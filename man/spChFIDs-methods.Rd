\name{spChFIDs-methods}
\docType{methods}
\alias{spChFIDs-methods}
\alias{spChFIDs<-}
\alias{spChFIDs<-,Spatial-method}
\alias{spChFIDs,SpatialLines,character-method}
\alias{spChFIDs,SpatialLinesDataFrame,character-method}
\alias{spChFIDs,SpatialPolygons,character-method}
\alias{spChFIDs,SpatialPolygonsDataFrame,character-method}
\title{change feature IDs in spatial objects}
\alias{spChFIDs}
\description{
 When the feature IDs need to be changed in SpatialLines* or SpatialPolygons* objects, these methods may be used. The new IDs should be a character vector of unique IDs of the correct length.
}
\section{Methods}{
\describe{

\item{obj = "SpatialLines", x = "character"}{replace IDs in a SpatialLines object}

\item{obj = "SpatialLinesDataFrame", x = "character"}{replace IDs in a SpatialLinesDataFrame object}

\item{obj = "SpatialPolygons", x = "character"}{replace IDs in a SpatialPolygons object}

\item{obj = "SpatialPolygonsDataFrame", x = "character"}{replace IDs in a SpatialPolygonsDataFrame object}
}}

\note{It is usually sensible to keep a copy of the original feature IDs in the object, but this should be done by the user.}

\author{Roger Bivand}

\seealso{\code{\link[maptools]{spCbind-methods}}, \code{\link[maptools]{spRbind-methods}}}

\examples{
\dontrun{
require(maptools)
xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
  IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
row.names(as(xx, "data.frame"))
xx1 <- spChFIDs(xx, as.character(xx$CNTY_ID))
row.names(as(xx1, "data.frame"))
}
}
\keyword{methods}
\keyword{spatial}
