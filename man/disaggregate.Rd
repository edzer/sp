\name{disaggregate-methods}
\docType{methods}
\alias{disaggregate}
\alias{disaggregate-methods}
\alias{disaggregate,SpatialPolygons-method}
\alias{disaggregate,SpatialPolygonsDataFrame-method}
\alias{disaggregate,SpatialLines-method}
\alias{disaggregate,SpatialLinesDataFrame-method}
\title{ disaggregate SpatialLines, SpatialLinesDataFrame, 
SpatialPolygons, or SpatialPolygonsDataFrame objects }
\description{ disaggregate SpatialLines, SpatialLinesDataFrame, 
SpatialPolygons, or SpatialPolygonsDataFrame objects, using functions from rgeos to handle polygon hole nesting }
\usage{ disaggregate(x, ...) }
\arguments{
\item{x}{object of class \link{SpatialLines} or \link{SpatialPolygons}}
\item{...}{ignored}
}
\value{ 
object of class \link{SpatialLines} or \link{SpatialPolygons},
where groups of \link{Line} or \link{Polygon} are disaggregated to
one \link{Line} per \link{Lines}, or one \link{Polygon}
per \link{Polygons}, respectively.
}

\author{ Robert Hijmans, Edzer Pebesma }
\keyword{methods}
