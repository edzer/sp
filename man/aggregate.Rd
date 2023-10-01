\name{aggregate}
\docType{methods}
\alias{aggregate}
\alias{aggregate.Spatial}

\title{ aggregation of spatial objects }
\description{ spatial aggregation of thematic information in spatial objects}
\usage{
\method{aggregate}{Spatial}(x, by = list(ID = rep(1, length(x))),
	FUN, \dots, dissolve = TRUE, areaWeighted = FALSE)
}
\arguments{
\item{x}{object deriving from \link{Spatial}, with attributes }
\item{by}{aggregation predicate; if \code{by} is a \link{Spatial} object,
the geometry by which attributes in \code{x} are aggregated; if \code{by}
is a list, aggregation by attribute(s), see \link{aggregate.data.frame}}
\item{FUN}{aggregation function, e.g. \link{mean}; see details}
\item{...}{arguments passed on to function \code{FUN}, unless \code{minDimension}
is specified, which is passed on to function \link{over}}
\item{dissolve}{logical; should, when aggregating based on attributes, the
resulting geometries be dissolved? Note that if \code{x} has class
\code{SpatialPointsDataFrame}, this returns an object of class \code{SpatialMultiPointsDataFrame}; deprecated}
\item{areaWeighted}{logical; should the aggregation of \code{x} be weighted by the
areas it intersects with each feature of \code{by}? See value; deprecated.}
}
\value{ 
The aggregation of attribute values of \code{x} either over the
geometry of \code{by} by using \link{over} for spatial matching,
or by attribute values, using aggregation function \code{FUN}.

If \code{areaWeighted} is \code{TRUE}, \code{FUN} is ignored and the
area weighted mean is computed for numerical variables, or if all
attributes are \code{factor}s, the area dominant factor level (area
mode) is returned.  This computes the intersection of \code{x}
and \code{by}; see examples below.  As this uses code from package
rgeos, it is deprecated as package rgeos will retire.

If \code{by} is missing, aggregates over all features.
}
\details{
For as far as these functions use package rgeos, (lines, polygons,
dissolve = TRUE), they are deprecated as rgeos will retire; try
using sf::aggregate instead.

\code{FUN} should be a function that takes as first argument a
vector, and that returns a single number. The canonical examples
are \link{mean} and \link{sum}. Counting features is obtained when
summing an attribute variable that has the value 1 everywhere.
}

\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\note{ uses \link{over} to find spatial match if \code{by} is a 
\link{Spatial} object }

\examples{
data("meuse")
coordinates(meuse) <- ~x+y
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
i = cut(meuse.grid$dist, c(0,.25,.5,.75,1), include.lowest = TRUE)
j = sample(1:2, 3103,replace=TRUE)

x = aggregate(meuse.grid["dist"], list(i=i,j=j), mean, dissolve = FALSE)
spplot(x["j"], col.regions=bpy.colors())


}
\keyword{methods}
