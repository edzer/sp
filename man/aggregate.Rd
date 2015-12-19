\name{aggregate}
\docType{methods}
\alias{aggregate}
\alias{aggregate.Spatial}

\title{ aggregation of spatial objects }
\description{ spatial aggregation of thematic information in spatial objects}
\usage{
\method{aggregate}{Spatial}(x, by = list(ID = rep(1, length(x))),
	FUN = mean, \dots, dissolve = TRUE, areaWeighted = FALSE)
}
\arguments{
\item{x}{object deriving from \link{Spatial}, with attributes }
\item{by}{aggregation predicate; if \code{by} is a \link{Spatial} object,
the geometry by which attributes in \code{x} are aggregated; if \code{by}
is a list, aggregation by attribute(s), see \link{aggregate.data.frame}}
\item{FUN}{aggregation function }
\item{...}{arguments passed on to function \code{FUN}, unless \code{minDimension}
is specified, which is passed on to function \link{over}}
\item{dissolve}{logical; should, when aggregating based on attributes, the
resulting geometries be dissolved? Note that if \code{x} has class
\code{SpatialPointsDataFrame}, this returns an object of class \code{SpatialMultiPointsDataFrame}}
\item{areaWeighted}{logical; should the aggregation of \code{x} be weighted by the
areas it intersects with each feature of \code{by}? See value.}
}
\value{ 
The aggregation of attribute values of \code{x} either over the
geometry of \code{by} by using \link{over} for spatial matching,
or by attribute values, using aggregation function \code{FUN}.

If \code{areaWeighted} is \code{TRUE}, \code{FUN} is ignored and the
area weighted mean is computed for numerical variables, or if all
attributes are \code{factor}s, the area dominant factor level (area
mode) is returned. This will compute the \link[rgeos]{gIntersection}
of \code{x} and \code{by}; see examples below.

If \code{by} is missing, aggregates over all features.
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
\dontrun{
if (require(rgeos)) {
	# aggregation by spatial object:
	ab = gUnaryUnion(as(meuse.grid, "SpatialPolygons"), meuse.grid$part.a)
	x = aggregate(meuse["zinc"], ab, mean)
	spplot(x)
	# aggregation by attribute, then dissolve to polygon:
	x = aggregate(meuse.grid["dist"], list(i=i))
	spplot(x["i"])
	x = aggregate(meuse.grid["dist"], list(i=i,j=j))
	spplot(x["dist"], col.regions=bpy.colors())
	spplot(x["i"], col.regions=bpy.colors(4))
	spplot(x["j"], col.regions=bpy.colors())
}
}

x = aggregate(meuse.grid["dist"], list(i=i,j=j), dissolve = FALSE)
spplot(x["j"], col.regions=bpy.colors())

if (require(gstat) && require(rgeos)) {
	x = idw(log(zinc)~1, meuse, meuse.grid, debug.level=0)[1]
	spplot(x[1],col.regions=bpy.colors())
	i = cut(x$var1.pred, seq(4, 7.5, by=.5), 
		include.lowest = TRUE)
	xa = aggregate(x["var1.pred"], list(i=i))
	spplot(xa[1],col.regions=bpy.colors(8))
}

if (require(rgeos)) {
# Area-weighted example, using two partly overlapping grids:

  gt1 = SpatialGrid(GridTopology(c(0,0), c(1,1), c(4,4)))
  gt2 = SpatialGrid(GridTopology(c(-1.25,-1.25), c(1,1), c(4,4)))

  # convert both to polygons; give p1 attributes to aggregate
  p1 = SpatialPolygonsDataFrame(as(gt1, "SpatialPolygons"), 
		  data.frame(v = 1:16, w=5:20, x=factor(1:16)), match.ID = FALSE)
  p2 = as(gt2, "SpatialPolygons")

  # plot the scene:
  plot(p1, xlim = c(-2,4), ylim = c(-2,4))
  plot(p2, add = TRUE, border = 'red')
  i = gIntersection(p1, p2, byid = TRUE)
  plot(i, add=TRUE, density = 5, col = 'blue')
  # plot IDs p2:
  ids.p2 = sapply(p2@polygons, function(x) slot(x, name = "ID"))
  text(coordinates(p2), ids.p2)
  # plot IDs i:
  ids.i = sapply(i@polygons, function(x) slot(x, name = "ID"))
  text(coordinates(i), ids.i, cex = .8, col = 'blue')

  # compute & plot area-weighted average; will warn for the factor
  ret = aggregate(p1, p2, areaWeighted = TRUE)
  spplot(ret)

  # all-factor attributes: compute area-dominant factor level:
  ret = aggregate(p1["x"], p2, areaWeighted = TRUE) 
  spplot(ret)
}
}
\keyword{methods}
