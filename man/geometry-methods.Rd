\name{geometry-methods}
\docType{methods}
\alias{geometry}
\alias{geometry-methods}
\alias{geometry,Spatial-method}
\alias{geometry,SpatialPointsDataFrame-method}
\alias{geometry,SpatialMultiPointsDataFrame-method}
\alias{geometry,SpatialGridDataFrame-method}
\alias{geometry,SpatialPixelsDataFrame-method}
\alias{geometry,SpatialLinesDataFrame-method}
\alias{geometry,SpatialPolygonsDataFrame-method}
\alias{geometry<-}
\alias{geometry<-,data.frame,Spatial-method}
\title{ Methods for retrieving the geometry from a composite (geometry + attributes) object }
\usage{ 
geometry(obj) 
geometry(obj) <- value 
}
\arguments{
\item{obj}{in case of assignment, a data.frame, else an object of class Spatial}
\item{value}{object of class Spatial}
}
\description{
geometry retrieves the SpatialXxx object from a SpatialXxxDataFrame object,
with Xxx Lines, Points, Polygons, Grid, or Pixels. geometry<- converts a
data.frame into a Spatial object.
}
\section{Methods}{
\describe{
\item{obj = "Spatial"}{ }
\item{obj = "SpatialPointsDataFrame"}{ }
\item{obj = "SpatialMultiPointsDataFrame"}{ }
\item{obj = "SpatialPolygonsDataFrame"}{ }
\item{obj = "SpatialPixelsDataFrame"}{ }
\item{obj = "SpatialGridDataFrame"}{ }
\item{obj = "SpatialLinesDataFrame"}{ }
}
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}
\examples{
data(meuse)
m = meuse
coordinates(m) = meuse[, c("x", "y")]
pts = geometry(m)
class(pts)
geometry(meuse) = pts
class(meuse)
identical(m, meuse) # TRUE
}
\keyword{methods}
