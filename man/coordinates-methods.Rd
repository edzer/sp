\name{coordinates-methods}
\docType{methods}
\alias{coordinates-methods}
% \alias{coordinates}
\alias{coordinates,list-method}
\alias{coordinates,data.frame-method}
\alias{coordinates,matrix-method}
\alias{coordinates,SpatialPoints-method}
\alias{coordinates,SpatialMultiPoints-method}
%\alias{coordinates,SpatialPointsDataFrame-method}
\alias{coordinates,SpatialPolygons-method}
\alias{coordinates,SpatialPolygonsDataFrame-method}
\alias{coordinates,Line-method}
\alias{coordinates,Lines-method}
\alias{coordinates,SpatialLines-method}
\alias{coordinates,GridTopology-method}
\alias{coordinates,SpatialPixels-method}
\alias{coordinates,SpatialPixelsDataFrame-method}
\alias{coordinates,SpatialGrid-method}
\alias{coordinates,SpatialGridDataFrame-method}
\alias{coordinates<-,data.frame-method}
\title{ retrieve (or set) spatial coordinates }
\description{ retrieve (or set) spatial coordinates from (for) spatial data }
\section{Methods}{
\describe{
\item{obj = "list"}{ list with (at least) two numeric components of equal length }
\item{obj = "data.frame"}{ data.frame with at least two numeric components }
\item{obj = "matrix"}{ numeric matrix with at least two columns }
\item{obj = "SpatialPoints"}{ object of, or deriving from, SpatialPoints }
\item{obj = "SpatialPointsDataFrame"}{ object of, or deriving from, 
	SpatialPointsDataFrame }
\item{obj = "SpatialPolygons"}{ object of, or deriving from, SpatialPolygons }
\item{obj = "SpatialPolygonsDataFrame"}{ object of, or deriving from, 
	SpatialPolygonsDataFrame }
\item{obj = "Line"}{ object of class Line; returned value is matrix }
\item{obj = "Lines"}{ object of class Lines; returned value is list of matrices }
\item{obj = "SpatialLines"}{ object of, or deriving from, SpatialLines; returned
value is list of lists of matrices }
\item{obj = "GridTopology"}{ object of, or deriving from, GridTopology }
\item{obj = "GridTopology"}{ object of, or deriving from, GridTopology }
\item{obj = "SpatialPixels"}{ object of, or deriving from, SpatialPixels }
\item{obj = "SpatialPixelsDataFrame"}{ object of, or deriving from, SpatialPixelsDataFrame }
\item{obj = "SpatialGrid"}{ object of, or deriving from, SpatialGrid }
\item{obj = "SpatialGridDataFrame"}{ object of, or deriving from, SpatialGridDataFrame }
}}
\section{Methods for "coordinates<-"}{
\describe{
\item{object = "data.frame", value="ANY"}{ promote data.frame to object of class
\link{SpatialPointsDataFrame-class}, by specifying coordinates; see \link{coordinates}}
}}
\keyword{methods}
