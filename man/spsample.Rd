\name{spsample}
\docType{methods}
\alias{spsample-methods}
\alias{spsample,Spatial-method}
\alias{spsample,Line-method}
\alias{spsample,Lines-method}
\alias{spsample,SpatialLines-method}
\alias{spsample,Polygon-method}
\alias{spsample,Polygons-method}
\alias{spsample,SpatialGrid-method}
\alias{spsample,SpatialPixels-method}
\alias{spsample,SpatialPolygons-method}
\alias{spsample}
\alias{makegrid}

\title{ sample point locations in (or on) a spatial object }

\description{ sample point locations within a square area, a grid,
a polygon, or on a spatial line, using regular or random sampling
methods; the methods used assume that the geometry used is not spherical, 
so objects should be in planar coordinates }
\usage{
spsample(x, n, type, ...)
makegrid(x, n = 10000, nsig = 2, cellsize, offset = rep(0.5, nrow(bb)),
	pretty = TRUE)
}
\arguments{
\item{x}{Spatial object; \code{spsample(x,...)} is a generic method for the
existing \code{sample.Xxx} functions}
\item{...}{ optional arguments, passed to the appropriate \code{sample.Xxx}
functions; see NOTES for \code{nclusters} and \code{iter}}
\item{n}{ (approximate) sample size }
\item{type}{ character; \code{"random"} for completely spatial random;
\code{"regular"} for regular (systematically aligned) sampling;
\code{"stratified"} for stratified random (one single random location in
each "cell"); \code{"nonaligned"} for nonaligned systematic sampling
(nx random y coordinates, ny random x coordinates); \code{"hexagonal"}
for sampling on a hexagonal lattice; \code{"clustered"} for clustered sampling;
 \code{"Fibonacci"} for Fibonacci sampling on the sphere (see references).}
\item{bb}{ bounding box of the sampled domain; setting this to a smaller
value leads to sub-region sampling }
\item{offset}{ for square cell-based sampling types (regular, stratified, 
nonaligned, hexagonal): the offset (position) of the regular
grid; the default for \code{spsample} methods is a random location in
the unit cell [0,1] x [0,1], leading to a different grid after
each call; if this is set to \code{c(0.5,0.5)}, the returned grid is
not random (but, in Ripley's wording, "centric systematic"). For
line objects, a single offset value is taken, where the value varies within 
the [0, 1] interval, and 0 is the beginning of each Line object, and 1 
its end }
\item{cellsize}{ if missing, a cell size is derived from the sample size
\code{n}; otherwise, this cell size is used for all sampling methods
except \code{"random"} }
\item{nsig}{ for "pretty" cell size; \code{spsample} does not result in
  pretty grids }
\item{pretty}{logical; if \code{TRUE}, choose pretty (rounded) coordinates}
}

\value{ an object of class \link{SpatialPoints-class}. The number of
points is only guaranteed to equal \code{n} when sampling is done in a
square box, i.e. (\code{sample.Spatial}). Otherwise, the obtained number
of points will have expected value \code{n}. 

When \code{x} is of a class deriving from \link{Spatial-class} for which
no \link{spsample-methods} exists, sampling is done in the bounding box
of the object, using \code{spsample.Spatial}. An overlay using
\link{over} may be necessary to select the features inside the geometry
afterwards. 

Sampling type \code{"nonaligned"} is not implemented for line objects. 

Some methods may return NULL if no points could be successfully placed. 

\code{makegrid} makes a regular grid that covers \code{x}; when
\code{cellsize} is not given it derives one from the number of grid
points requested (approximating the number of cells). It tries to
choose pretty cell size and grid coordinates.

}

\section{Methods}{
\describe{
\item{x = "Spatial"}{ sample in the bbox of \code{x} }
\item{x = "Line"}{ sample on a line }
\item{x = "Polygon"}{ sample in a Polygon }
\item{x = "Polygons"}{ sample in a Polygons object, consisting of possibly
multiple Polygon objects (holes must be correctly defined, use \code{checkPolygonsHoles} if need be) }
\item{x = "SpatialPolygons"}{ sample in an SpatialPolygons object; sampling
takes place over all Polygons objects present, use subsetting to vary
sampling intensity (density); holes must be correctly defined, use \code{checkPolygonsHoles} if need be }
\item{x = "SpatialGrid"}{ sample in an SpatialGrid object }
\item{x = "SpatialPixels"}{ sample in an SpatialPixels object }
}}

\note{If an \link{Polygon-class} object has zero area (i.e. is a line),
samples on this line element are returned. If the area is very close
to zero, the algorithm taken here (generating points in a square
area, selecting those inside the polygon) may be very resource
intensive. When numbers of points per polygon are small and
type="random", the number searched for is inflated to ensure hits,
and the points returned sampled among these.

The following two arguments can be further specified:

\code{nclusters} Number of clusters (strata) to sample from.

\code{iter}(default = 4) number of times to try to place sample points
  in a polygon before giving up and returning NULL - this may occur when
  trying to hit a small and awkwardly shaped polygon in a large bounding
  box with a small number of points

}

\references{Chapter 3 in B.D. Ripley, 1981. Spatial Statistics, Wiley

Fibonacci sampling:  Alvaro Gonzalez, 2010. Measurement of Areas on a
Sphere Using Fibonacci and Latitude-Longitude Lattices. Mathematical
Geosciences 42(1), p. 49-64 }

\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\seealso{\link{over}, \link{point.in.polygon}, \link{sample}}
\examples{

data(meuse.riv)
meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), "x")))

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "regular"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "random"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "stratified"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "nonaligned"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr@polygons[[1]], n = 100, "stratified"), pch = 3, cex=.5)

data(meuse.grid)
gridded(meuse.grid) = ~x+y
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="random"), pch=3, cex=.5)
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="stratified"), pch=3, cex=.5)
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="regular"), pch=3, cex=.5)
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="nonaligned"), pch=3, cex=.5)

fullgrid(meuse.grid) = TRUE
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="stratified"), pch=3,cex=.5)

}
\keyword{manip}
\keyword{methods}
