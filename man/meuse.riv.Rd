\name{meuse.riv}
\alias{meuse.riv}
\alias{meuse.area}
\docType{data}
\title{ River Meuse outline }
\usage{data(meuse.riv)
data(meuse.area)}
\description{
The \code{meuse.riv} data consists of an outline of the Meuse
river in the area a few kilometers around the \link{meuse}
data set. 

The \code{meuse.area} polygon has an outline of \link{meuse.grid}. See
example below how it can be created from \link{meuse.grid}.
}
\format{
\code{meuse.riv}: two-column data.frame containing 176 coordinates.

\code{meuse.area}: two-column matrix with coordinates of outline.
}
\details{
\code{x} and \code{y} are in RDM, the Dutch topographical map
coordinate system. See examples of \code{spTransform} in the
rgdal package for projection parameters.
}
\references{
See the \link{meuse} documentation
}
\examples{
data(meuse.riv)
plot(meuse.riv, type = "l", asp = 1)
data(meuse.grid)
coordinates(meuse.grid) = c("x", "y")
gridded(meuse.grid) = TRUE
image(meuse.grid, "dist", add = TRUE)
data(meuse)
coordinates(meuse) = c("x", "y")
meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
spplot(meuse.grid, col.regions=bpy.colors(), main = "meuse.grid",
  sp.layout=list(
	list("sp.polygons", meuse.sr),
	list("sp.points", meuse, pch="+", col="black")
  )
)
spplot(meuse, "zinc", col.regions=bpy.colors(),  main = "zinc, ppm",
  cuts = c(100,200,400,700,1200,2000), key.space = "right",
  sp.layout= list("sp.polygons", meuse.sr, fill = "lightblue")
)

# creating meuse.area from meuse.grid:
if (require(rgeos)) {
    meuse.area = gUnaryUnion(as(meuse.grid, "SpatialPolygons"))
	plot(meuse.area)
}

}
\keyword{datasets}
