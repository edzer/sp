\name{polygons-methods}
\docType{methods}
\alias{polygons-methods}
\alias{polygons,Spatial-method}
\alias{polygons,SpatialPolygons-method}
\alias{polygons<-,data.frame,SpatialPolygons-method}
\title{ Retrieve polygons from SpatialPolygonsDataFrame object }
\description{
Retrieve polygons from SpatialPolygonsDataFrame object
}
\section{Methods for polygons}{
\describe{
\item{obj = "SpatialPolygons"}{ object of, or deriving from, SpatialPolygons }
\item{obj = "SpatialPolygonsDataFrame"}{ object of, or deriving from, 
	SpatialPolygonsDataFrame }
}}

\section{Methods for "polygons<-"}{
\describe{
\item{object = "data.frame", value="SpatialPolygons"}{ promote data.frame to object of class
\link{SpatialPolygonsDataFrame-class}, by specifying polygons }
}}
\keyword{methods}
\keyword{spatial}
