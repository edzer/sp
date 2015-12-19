\name{SpatialPixelsDataFrame}
\alias{SpatialPixelsDataFrame}
\alias{SpatialGridDataFrame}

\title{ define spatial grid with attribute data }
\description{ defines spatial grid by offset, cell size and dimensions }
\usage{
SpatialPixelsDataFrame(points, data, tolerance = sqrt(.Machine$double.eps), 
		proj4string = CRS(as.character(NA)), round = NULL, grid = NULL)
SpatialGridDataFrame(grid, data, proj4string = CRS(as.character(NA)))
}
\arguments{
	\item{points}{ coordinates, either as numeric matrix or as object of class
		\link{SpatialPoints-class} }
	\item{grid}{ grid topology; object of class \link{GridTopology-class}; for
	calls to \code{SpatialPixelsDataFrame} a value of NULL implies that this will 
	be derived from the point coordinates }
	\item{data}{ data.frame; contains the attribute (actual grid) data }
	\item{tolerance}{ precision up to which extent points should be 
	exactly on a grid }
        \item{round}{default NULL, otherwise a value passed to as the digits argument to \code{round} for setting cell size}
    \item{proj4string}{object of class \link{CRS-class} in the first
	form only used when \code{points} does not inherit from \link{Spatial-class}}
}

\value{ SpatialPixelsDataFrame returns an object of class
\link{SpatialPixelsDataFrame-class}; SpatialGridDataFrame returns an object
of class \link{SpatialGridDataFrame-class}. 
}

\note{ 
SpatialPixels stores grid topology and coordinates of the actual points,
which may be in the form of a subset (set of pixels) of a full grid. To
find out or change this, see \link{fullgrid} and \link{SpatialGrid-class}.
}


\author{ Edzer Pebesma }

\seealso{ 
\link{gridded},
\link{gridded<-},
\link{SpatialGrid},
\link{SpatialGrid-class}
}

\examples{
data(meuse.grid)
m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)
class(m)
summary(m)
}

\keyword{spatial}
