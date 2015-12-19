\name{SpatialPixels}
\alias{SpatialPixels}
\alias{SpatialGrid}
\alias{GridTopology}
\alias{SpatialGrid}
\alias{plot.SpatialGrid}
\alias{coordinatevalues}
\alias{points2grid}
\alias{getGridIndex}
\alias{getGridTopology}
\alias{areaSpatialGrid}
\title{ define spatial grid }
\description{
defines spatial grid by offset, cell size and dimensions
}
\usage{
GridTopology(cellcentre.offset, cellsize, cells.dim)
SpatialPixels(points, tolerance = sqrt(.Machine$double.eps),
		proj4string = CRS(as.character(NA)), round = NULL, grid = NULL)
SpatialGrid(grid, proj4string = CRS(as.character(NA)))
coordinatevalues(obj)
points2grid(points, tolerance = sqrt(.Machine$double.eps), round=NULL)
getGridIndex(cc, grid, all.inside = TRUE)
getGridTopology(obj)
areaSpatialGrid(obj)
}
\arguments{
    \item{cellcentre.offset}{numeric; vector with the smallest coordinates for
    each dimension}
    \item{cellsize}{numeric; vector with the cell size in each dimension}
    \item{cells.dim}{integer; vector with number of cells in each dimension}
	\item{points}{ coordinates, object of class \link{SpatialPoints-class} }
	\item{grid}{ grid topology; object of class \link{GridTopology-class};
	for calls to \code{SpatialPixels}, a value of NULL implies that this will 
	be derived from the point coordinates }
	\item{tolerance}{ precision, used to which extent points are exactly on
	a grid}
        \item{round}{default NULL, otherwise a value passed to as the digits argument to \code{round} for setting cell size}
	\item{proj4string}{ object of class \link{CRS-class}}
	\item{obj}{object of class or deriving from \link{SpatialGrid-class}}
	\item{cc}{ numeric matrix with coordinates }
	\item{all.inside}{ logical; if TRUE and \code{cc} points fall outside
	the grid area, an error message is generated; if FALSE, NA values
	are generated for such points }
}

\value{ GridTopology returns a value of class \link{GridTopology-class};
SpatialGrid returns an object of class \link{SpatialGrid-class}

\code{coordinatevalues} returns a list with the unique x-coordinates,
the unique y-coordinate, etc. instead of the \link{coordinates} of all
grid cells

SpatialGrid returns an object of class \link{SpatialGrid-class}. 

points2grid returns the \link{GridTopology-class} from a set of points.

getGridIndex finds the index of a set of point coordinates in a given
grid topology, and depending on \code{all.inside} setting, generates NA
or an error message if points are outside the grid domain.

getGridTopology returns the slot of class \link{GridTopology-class} from
obj.

areaSpatialGrid returns the spatial area of (the non-missing valued cells
of) the grid. For objects of class \link{SpatialGridDataFrame-class}
the area refers to cells where any (one or more) of the attribute columns
are non-missing valued.
}

\note{ 
SpatialGrid stores grid topology and may or may not store the coordinates
of the actual points, which may form a subset of the full grid. To find
out or change this, see \link{fullgrid}.

points2grid tries to figure out the grid topology from points. It succees
only if points on a grid line have constant y column, and points on a
grid column have constant x coordinate, etc. In other cases, use signif
on the raw coordinate matrices to make sure this is the case.
}


\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\seealso{ 
\link{SpatialGrid-class},
\link{SpatialGridDataFrame-class},
}

\examples{
x = GridTopology(c(0,0), c(1,1), c(5,4))
class(x)
x
summary(x)
coordinates(x)
coordinates(GridTopology(c(0,0), c(1,1), c(5,4)))
coordinatevalues(x)
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
points2grid(meuse.grid)
data(meuse.grid)
set.seed(1)
meuse.grid$x <- meuse.grid$x + rnorm(length(meuse.grid$x), 0, 0.002)
meuse.grid$y <- meuse.grid$y + rnorm(length(meuse.grid$y), 0, 0.002)
coordinates(meuse.grid) <- c("x", "y")
#EJP
# points2grid(meuse.grid, tolerance=0.76, round=1)
data(meuse.grid)
a <- which(meuse.grid$x == 180140)
b <- which(meuse.grid$x == 180180)
c <- which(meuse.grid$x == 179260)
d <- which(meuse.grid$y == 332460)
e <- which(meuse.grid$y == 332420)
f <- which(meuse.grid$y == 330740)
meuse.grid <- meuse.grid[-c(a, b, c, d, e, f),]
coordinates(meuse.grid) <- c("x", "y")
points2grid(meuse.grid)
data(meuse.grid)
set.seed(1)
meuse.grid$x <- meuse.grid$x + rnorm(length(meuse.grid$x), 0, 0.002)
meuse.grid$y <- meuse.grid$y + rnorm(length(meuse.grid$y), 0, 0.002)
meuse.grid <- meuse.grid[-c(a, b, c, d, e, f),]
coordinates(meuse.grid) <- c("x", "y")
# EJP
# points2grid(meuse.grid, tolerance=0.69, round=1)
}

\keyword{spatial}
