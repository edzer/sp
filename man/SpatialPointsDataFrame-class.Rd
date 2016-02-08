\name{SpatialPointsDataFrame-class}
\docType{class}
\alias{SpatialPointsDataFrame-class}
\alias{[,SpatialPointsDataFrame-method}
\alias{coerce,SpatialPointsDataFrame,data.frame-method}
\alias{coerce,SpatialPointsDataFrame,SpatialPoints-method}
\alias{show,SpatialPointsDataFrame-method}
\alias{points,SpatialPointsDataFrame-method}
\alias{coordinates,SpatialPointsDataFrame-method}
\alias{as.data.frame.SpatialPointsDataFrame}
\alias{dim.SpatialPointsDataFrame}
\alias{print.SpatialPointsDataFrame}
\alias{ShowSpatialPointsDataFrame}
\alias{as.SpatialPoints.SpatialPointsDataFrame}
\alias{rbind.SpatialPointsDataFrame}
\alias{row.names.SpatialPointsDataFrame}
\alias{coerce,ppp,SpatialPointsDataFrame-method}

\title{Class "SpatialPointsDataFrame" }
\description{ Class for spatial attributes that have spatial point locations }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{coordinates(x) = c("x", "y") }.
or of the form \code{coordinates(x) = xy}; see \link{coordinates}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class data.frame containing
		the attribute data (may or may not contain the coordinates in its columns) }
    \item{\code{coords}:}{Object of class \code{"matrix"}; the coordinates matrix
	(points are rows in the matrix) }
	\item{\code{coords.nrs}}{Object of class logical; if TRUE, when
	the object was created the coordinates were retrieved from the data.frame,
	and hence stripped from it; after coercion to data.frame, e.g. by 
	as.data.frame(x), coordinates will again be added (as first few columns) 
	to the data.frame }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection string}
  }
}
\section{Extends}{
Class \code{"SpatialPoints"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
}
\usage{
  \S4method{[}{SpatialPointsDataFrame}(x, i, j, ..., drop = TRUE)
  \S4method{coerce}{SpatialPointsDataFrame,SpatialPoints}(from, to, strict=TRUE)
  \S4method{coerce}{SpatialPointsDataFrame,data.frame}(from, to, strict=TRUE)
  \S4method{coordinates}{SpatialPointsDataFrame}(obj)
  \S4method{show}{SpatialPointsDataFrame}(object)
  \S4method{points}{SpatialPointsDataFrame}(x)
  \method{rbind}{SpatialPointsDataFrame}(...)
}
\arguments{
  \item{x,from,obj,object}{\code{SpatialPointsDataFrame} object}
  \item{to}{class to which to coerce}
  \item{strict}{see \code{\link[methods]{as}}}
  \item{i}{row indices}
  \item{j}{column indices}
  \item{drop}{see \code{\link[base]{Extract}}}
  \item{...}{indices passed through}
}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{coordinates}}, \code{\link{SpatialPoints-class}} 
}
\examples{
data(meuse)
xy = meuse[c("x", "y")] # retrieve coordinates as data.frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c("x", "y") # specify column names
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c(1, 2) # specify column names
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = ~x+y # formula
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = xy   # as data frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = as.matrix(xy)   # as matrix
meuse$log.zn = log(meuse$zinc)
class(meuse)
dim(meuse)
}
\keyword{classes}
