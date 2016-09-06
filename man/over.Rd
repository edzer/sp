\name{over-methods}
\docType{methods}
\alias{over}
\alias{\%over\%}
\alias{over-methods}
\alias{over,SpatialPoints,SpatialPoints-method}
\alias{over,SpatialPoints,SpatialPointsDataFrame-method}
\alias{over,SpatialPoints,SpatialPolygons-method}
\alias{over,SpatialPoints,SpatialPolygonsDataFrame-method}
\alias{over,SpatialGridDataFrame,SpatialPolygonsDataFrame-method}
\alias{over,SpatialPolygons,SpatialPoints-method}
\alias{over,SpatialPolygons,SpatialPointsDataFrame-method}
\alias{over,SpatialGridDataFrame,SpatialPoints-method}
\alias{over,SpatialGrid,SpatialPoints-method}
\alias{over,SpatialPixelsDataFrame,SpatialPoints-method}
\alias{over,SpatialPixels,SpatialPoints-method}
\alias{over,SpatialPoints,SpatialGrid-method}
\alias{over,SpatialPoints,SpatialGridDataFrame-method}
\alias{over,SpatialPoints,SpatialPixels-method}
\alias{over,SpatialPoints,SpatialPixelsDataFrame-method}
\alias{over,SpatialPolygons,SpatialGridDataFrame-method}
\alias{over,SpatialGrid,SpatialPolygons-method}
\alias{over,SpatialGrid,SpatialPolygonsDataFrame-method}
\alias{over,SpatialPolygons,SpatialGrid-method}
\alias{over,SpatialGrid,SpatialGrid-method}
\alias{over,SpatialGrid,SpatialGridDataFrame-method}
\alias{over,SpatialGrid,SpatialPixels-method}
\alias{over,SpatialGrid,SpatialPixelsDataFrame-method}
\alias{over,SpatialGrid,SpatialPointsDataFrame-method}
\alias{over,Spatial,Spatial-method}
\alias{overDF_for_rgeos}

\title{ consistent spatial overlay for points, grids and polygons }
\description{ consistent spatial overlay for points, grids and
polygons: at the spatial locations of object x retrieves the indexes
or attributes from spatial object y }
\usage{
over(x, y, returnList = FALSE, fn = NULL, ...)
x \%over\% y
}
\arguments{
\item{x}{geometry (locations) of the queries}
\item{y}{layer from which the geometries or attributes are queried}
\item{returnList}{ logical; see value }
\item{fn}{(optional) a function; see value }
\item{...}{arguments passed on to function \code{fn}, except for the special
argument \code{minDimension}: minimal dimension for an intersection to be counted;
-1 takes any intersection, and does not order; 0 takes any intersection
but will order according to dimensionality of the intersections (if returnList
is TRUE, 1 (2) selects intersections with dimension 1, meaning
lines (2, meaning areas); see \code{vignette("over")} for details }
}
\value{ 
If \code{y} is only geometry an object of length \code{length(x)}.
If \code{returnList} is \code{FALSE}, a vector with the (first) index
of \code{y} for each geometry (point, grid cell centre, polygon
or lines) matching \code{x}.  if \code{returnList} is TRUE, a list of
length \code{length(x)}, with list element \code{i} the vector of
all indices of the geometries in \code{y} that correspond to the
$i$-th geometry in \code{x}.

If \code{y} has attribute data, attribute data are
returned. \code{returnList} is FALSE, a \code{data.frame} with
number of rows equal to \code{length(x)} is returned, if it is
TRUE a list with \code{length(x)} elements is returned, with a list
element the \code{data.frame} elements of all geometries in \code{y}
that correspond to that element of \code{x}.

In case the \code{rgeos} \code{over} methods are used,
matching is done by \link[rgeos]{gRelate}, which uses DE-9IM
(\url{https://en.wikipedia.org/wiki/DE-9IM}). From the string
returned, characters 1, 2, 4 and 5 are used, indicating the dimension
of the overlap of the inner and boundary of each \code{x} geometry
with the inner and boundary of each \code{y} geometry.  The order
in which matched \code{y} geometries are returned is determined by
the dimension of the overlap (2: area overlap, 1: line in common,
0: point in common), and then by the position in the string (1,
2, 4, 5, meaning points in polygons are prefered over points on
polygon boundaries). 
}

\section{Methods}{
\describe{
\item{x = "SpatialPoints", y = "SpatialPolygons"}{ returns a numeric
vector of length equal to the number of points; the number is the
index (number) of the polygon of \code{y} in which a point falls;
NA denotes the point does not fall in a polygon; if a point falls
in multiple polygons, the last polygon is recorded.  }
\item{x = "SpatialPointsDataFrame", y = "SpatialPolygons"}{ equal
to the previous method, except that an argument \code{fn=xxx} is
allowed, e.g. \code{fn = mean} which will then report a data.frame
with the mean attribute values of the \code{x} points falling 
in each polygon (set) of \code{y} }
\item{x = "SpatialPoints", y = "SpatialPolygonsDataFrame"}{ returns
a data.frame of the second argument with row entries corresponding
to the first argument }
\item{x = "SpatialPolygons", y = "SpatialPoints"}{ returns
the polygon index of points in \code{y}; if \code{x} is
a \code{SpatialPolygonsDataFrame}, a data.frame with rows from
\code{x} corresponding to points in \code{y} is returned.}
\item{x = "SpatialGridDataFrame", y = "SpatialPoints"}{ returns
object of class SpatialPointsDataFrame with grid attribute values
x at spatial point locations y; NA for NA grid cells or points
outside grid, and NA values on NA grid cells. }
\item{x = "SpatialGrid", y = "SpatialPoints"}{ returns grid values
x at spatial point locations y; NA for NA grid cells or points
outside the grid }
\item{x = "SpatialPixelsDataFrame", y = "SpatialPoints"}{ returns
grid values x at spatial point locations y; NA for NA grid cells
or points outside the grid }
\item{x = "SpatialPixels", y = "SpatialPoints"}{ returns grid
values x at spatial point locations y; NA for NA grid cells or
points outside the grid }
\item{x = "SpatialPoints", y = "SpatialGrid"}{ xx }
\item{x = "SpatialPoints", y = "SpatialGridDataFrame"}{ xx }
\item{x = "SpatialPoints", y = "SpatialPixels"}{ xx }
\item{x = "SpatialPoints", y = "SpatialPixelsDataFrame"}{ xx }
\item{x = "SpatialPolygons", y = "SpatialGridDataFrame"}{ xx }
}
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\note{\code{over} can be seen as a left outer join in SQL; the
match is a spatial intersection.

points on a polygon boundary and points corresponding to a
polygon vertex are considered to be inside the polygon.

These methods assume that pixels and grid cells are never
overlapping; for objects of class \code{SpatialPixels} this is
not guaranteed.

\code{over} methods that involve \code{SpatialLines} objects, or
pairs of \code{SpatialPolygons} require package \code{rgeos},
and use \link[rgeos]{gIntersects}.}

\seealso{
\code{vignette("over")} for examples and figures;
\link{point.in.polygon}, package \link[rgeos]{gIntersects}}
\examples{
r1 = cbind(c(180114, 180553, 181127, 181477, 181294, 181007, 180409, 
180162, 180114), c(332349, 332057, 332342, 333250, 333558, 333676, 
332618, 332413, 332349))
r2 = cbind(c(180042, 180545, 180553, 180314, 179955, 179142, 179437, 
179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683, 
331133, 331623, 332152, 332357, 332373))
r3 = cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 179875, 
179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004, 
329783, 329665, 329720, 329933, 330478, 331062, 331086))
r4 = cbind(c(180304, 180403,179632,179420,180304),
c(332791, 333204, 333635, 333058, 332791))

sr1=Polygons(list(Polygon(r1)),"r1")
sr2=Polygons(list(Polygon(r2)),"r2")
sr3=Polygons(list(Polygon(r3)),"r3")
sr4=Polygons(list(Polygon(r4)),"r4")
sr=SpatialPolygons(list(sr1,sr2,sr3,sr4))
srdf=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:4,5:2), 
	row.names=c("r1","r2","r3","r4")))

data(meuse)
coordinates(meuse) = ~x+y

plot(meuse)
polygon(r1)
polygon(r2)
polygon(r3)
polygon(r4)
# retrieve mean heavy metal concentrations per polygon:
over(sr, meuse[,1:4], fn = mean)

# return the number of points in each polygon:
sapply(over(sr, geometry(meuse), returnList = TRUE), length)

data(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE

over(sr, geometry(meuse))
over(sr, meuse)
over(sr, geometry(meuse), returnList = TRUE)
over(sr, meuse, returnList = TRUE)

over(meuse, sr)
over(meuse, srdf)

# same thing, with grid:
over(sr, meuse.grid)
over(sr, meuse.grid, fn = mean)
over(sr, meuse.grid, returnList = TRUE)

over(meuse.grid, sr)
over(meuse.grid, srdf, fn = mean)
over(as(meuse.grid, "SpatialPoints"), sr)
over(as(meuse.grid, "SpatialPoints"), srdf)
}
\keyword{methods}
