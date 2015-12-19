\name{SpatialMultiPointsDataFrame-class}
\docType{class}
\alias{SpatialMultiPointsDataFrame-class}
\alias{[,SpatialMultiPointsDataFrame-method}
\alias{coerce,SpatialMultiPointsDataFrame,data.frame-method}
%\alias{coerce,SpatialMultiPointsDataFrame,SpatialMultiPoints-method}
\alias{coerce,SpatialMultiPointsDataFrame,SpatialPointsDataFrame-method}
\alias{show,SpatialMultiPointsDataFrame-method}
\alias{points,SpatialMultiPointsDataFrame-method}
\alias{coordinates,SpatialMultiPointsDataFrame-method}
\alias{as.data.frame.SpatialMultiPointsDataFrame}
\alias{dim.SpatialMultiPointsDataFrame}
\alias{print.SpatialMultiPointsDataFrame}
%\alias{as.SpatialMultiPoints.SpatialMultiPointsDataFrame}
\alias{rbind.SpatialMultiPointsDataFrame}
\alias{row.names.SpatialMultiPointsDataFrame}

\title{Class "SpatialMultiPointsDataFrame" }
\description{ Class for spatial attributes that correspond to point sets }
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class data.frame containing
		the attribute data (may or may not contain the coordinates in its columns) }
    \item{\code{coords}:}{Object of class \code{"list"}; the list with coordinates matrices;
	points are rows in the matrix, the list length equals the number of rows in the \code{data} slot }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection string}
  }
}
\section{Extends}{
Class \code{"SpatialMultiPoints"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialMultiPoints"}.
}
\usage{
  \S4method{[}{SpatialMultiPointsDataFrame}(x, i, j, ..., drop = TRUE)
  \S4method{coerce}{SpatialMultiPointsDataFrame,data.frame}(from, to, strict=TRUE)
  \S4method{coordinates}{SpatialMultiPointsDataFrame}(obj)
  \S4method{show}{SpatialMultiPointsDataFrame}(object)
  \S4method{points}{SpatialMultiPointsDataFrame}(x)
}
\arguments{
  \item{x,from,obj,object}{\code{SpatialMultiPointsDataFrame} object}
  \item{to}{class to which to coerce}
  \item{strict}{see \code{\link[methods]{as}}}
  \item{i}{row indices}
  \item{j}{column indices}
  \item{drop}{see \code{\link[base]{Extract}}}
  \item{...}{indices passed through}
}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{coordinates}}, \code{\link{SpatialMultiPoints-class}} 
}
\examples{
# create three sets of points:
cl1 = cbind(rnorm(3, 10), rnorm(3, 10))
cl2 = cbind(rnorm(5, 10), rnorm(5,  0))
cl3 = cbind(rnorm(7,  0), rnorm(7, 10))

mpdf = SpatialMultiPointsDataFrame(list(cl1, cl2, cl3), data.frame(a = 1:3))
mpdf

plot(mpdf, col = mpdf$a, cex = 1:3)
as(mpdf, "data.frame")
mpdf[1:2,]

}
\keyword{classes}
