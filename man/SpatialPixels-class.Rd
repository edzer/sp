\name{SpatialPixels-class}
\docType{class}
\alias{SpatialPixels-class}
\alias{[,SpatialPixels-method}
\alias{coerce,SpatialPixels,SpatialGrid-method}
\alias{print.summary.SpatialPixels}
\alias{as.data.frame.SpatialPixels}
\alias{summary,SpatialPixels-method}
\alias{coerce,SpatialPixels,data.frame-method}
\alias{show,SpatialPixels-method}
\alias{plot,SpatialPixels,missing-method}
\alias{rbind.SpatialPixels}

\title{Class "SpatialPixels" }
\description{ class for defining a pixels, forming a
possibly incomplete rectangular grid of arbitrary dimension }
\section{Objects from the Class}{
Objects are created by using e.g. 

SpatialPixels(points)

with points of class \link{SpatialPoints-class}
}
\section{Slots}{
  \describe{
	\item{\code{grid}}{ object of class \link{GridTopology-class}, defining the
	grid topology (offset, cellsize, dim) }
	\item{\code{grid.index}}{integer; index of points in full grid}
	\item{\code{coords}}{ coordinates of points, or bbox of grid }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection  }
  }
}
\section{Extends}{
Class \code{"SpatialPoints"} directly;
Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
}
\section{Methods}{
 \describe{
   \item{coordinates}{\code{signature(x = "SpatialPixels")}: calculates coordinates for
   each point on the grid; coordinates are not stored in objects of class SpatialGrid}
   \item{summary}{\code{signature(object = "SpatialPixels")}: summarize object}
   \item{plot}{\code{signature(x = "SpatialPixels")}: plots cell centers }
   \item{"["}{\code{signature(x = "SpatialPixels")}: select pixel cells; the argument
   \code{drop=FALSE} (default) does not recalculate grid topology for the selection, if
   \code{drop=TRUE} the grid topology is recomputed, and might change. }
   \item{rbind}{\code{signature(x = "SpatialPixels")}: rbind-like method}
 }
}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{SpatialPixelsDataFrame-class}}, \code{\link{SpatialGrid-class}}
}
\examples{
data(meuse.grid)
pts = meuse.grid[c("x", "y")]
y = SpatialPixels(SpatialPoints(pts))
class(y)
y
summary(y)
plot(y) # plots grid
plot(y, grid = FALSE) # plots points
}
\keyword{classes}
