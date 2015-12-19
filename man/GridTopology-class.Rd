\name{GridTopology-class}
\docType{class}
\alias{GridTopology-class}
\alias{print.summary.GridTopology}
\alias{summary,GridTopology-method}
\alias{show,GridTopology-method}
\alias{show,summary.GridTopology-method}
\alias{coerce,GridTopology,data.frame-method}
\alias{coerce,SpatialGrid,GridTopology-method}
\alias{coerce,SpatialPixels,GridTopology-method}

\title{Class "GridTopology" }
\description{ class for defining a rectangular grid of
arbitrary dimension }
\section{Objects from the Class}{
Objects are created by using e.g. 

GridTopology(c(0,0), c(1,1), c(5,5))

see \link{SpatialGrid}
}
\section{Slots}{
  \describe{
    \item{\code{cellcentre.offset}:}{numeric; vector with the smallest coordinates for
	each dimension; coordinates refer to the cell centre }
    \item{\code{cellsize}:}{numeric; vector with the cell size in each dimension}
    \item{\code{cells.dim}:}{integer; vector with number of cells in each dimension}
  }
}

%\section{Extends}{
% Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
%}
\section{Methods}{
 \describe{
   \item{coordinates}{\code{signature(x = "SpatialGrid")}: calculates coordinates for
   each point on the grid}
   \item{summary}{\code{signature(object = "SpatialGrid")}: summarize object}
   \item{coerce}{\code{signature(from = "GridTopology", to = "data.frame")}: 
		convert to data.frame with columns cellcentre.offset, cellsize and cells.dim}
 }
}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{SpatialGridDataFrame-class}}, \code{\link{SpatialGrid-class}}
}
\examples{
x = GridTopology(c(0,0), c(1,1), c(5,5))
class(x)
x
summary(x)
coordinates(x)
y = SpatialGrid(grid = x)
class(y)
y
}
\keyword{classes}
