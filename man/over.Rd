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

}

\seealso{
\code{vignette("over")} for examples and figures}
\keyword{methods}
