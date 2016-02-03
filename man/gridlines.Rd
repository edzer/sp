\name{gridlines}
\alias{gridlines}
\alias{gridat}
\alias{text}
\alias{text.SpatialPointsDataFrame}
\alias{labels}
\alias{labels.SpatialLines}

\title{ Create N-S and E-W grid lines over a geographic region }
\description{ Create N-S and E-W grid lines over a geographic region; create and plot corresponding labels }
\usage{
gridlines(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]),
 ndiscr = 20) 
gridat(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]),
 offset = 0.5, side = "WS")
\method{labels}{SpatialLines}(object, newCRS, side = 1:2, ...)
\method{text}{SpatialPointsDataFrame}(x, ...)
}
\arguments{
  \item{x}{object deriving from class \link{Spatial-class}}
  \item{easts}{numeric; east-west values for vertical lines}
  \item{norths}{numeric; north-south values for horizontal lines}
  \item{ndiscr}{integer; number of points used to discretize the line, could
  be set to 2, unless the grid is (re)projected}
  \item{offset}{offset value to be returned, see \link{text}}
  \item{object}{\link{SpatialLines-class} object, as returned by \code{gridlines}}
  \item{newCRS}{the new (non-longlat) CRS in which the grid lines are being plotted; if missing, the CRS from \code{gridlines} are taken}
  \item{side}{for \code{labels}: integer, indicating side(s) at which gridlines labels will be drawn:
  1=below (S), 2=left (W), 3=above (N), and 4=right (E);
  for \code{gridat}: default \dQuote{WS}, if \dQuote{EN} labels placed on the top and right borders}
  \item{...}{ for \code{labels}: ignored; for \code{text}: arguments passed on 
  to \link[graphics]{text}, see below for example use of \code{adj}}
}

\value{ \code{gridlines} returns an object of class \link{SpatialLines-class}, with lines as specified;
the return object inherits the projection information of \code{x}; \code{gridat} returns a SpatialPointsDataFrame with points at the west and south ends of the grid lines created by \code{gridlines}, with degree labels.

The \code{labels} method for \code{SpatialLines} objects returns
a \code{SpatialPointsDataFrame} object with the parameters needed
to print labels below and left of the gridlines. This object is
prepared to be plotted with \code{text}:

The \code{text} method for \code{SpatialPointsDataFrame} puts text labels
on its coordinates, and takes care of attributes \code{pos}, \code{labels},
\code{srt} and \code{offset}; see \link[graphics]{text}.

}

\author{
  Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}, using example code
  of Roger Bivand.
}

\seealso{ \link[rgdal]{spTransform}; llgridlines in \code{rgdal} (recent versions) for plotting long-lat grid over projected data }
\examples{
data(meuse)
coordinates(meuse) = ~x+y
plot(meuse)
plot(gridlines(meuse), add = TRUE)
title("default gridlines within Meuse bounding box")

proj4string(meuse) <- CRS("+init=epsg:28992")
meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
grd <- gridlines(meuse_ll)
grd_x <- spTransform(grd, CRS("+init=epsg:28992"))

# labels South and West:
plot(meuse)
plot(grd_x, add=TRUE, lty=2)
grdat_ll <- gridat(meuse_ll)
grdat_x <- spTransform(grdat_ll, CRS("+init=epsg:28992"))
text(grdat_x)

# labels North and East:
plot(meuse)
plot(grd_x, add=TRUE, lty=2)
grdat_ll <- gridat(meuse_ll, side="EN")
grdat_x <- spTransform(grdat_ll, CRS("+init=epsg:28992"))
text(grdat_x)

# now using labels:
plot(meuse)
plot(grd_x, add=TRUE, lty=2)
text(labels(grd, CRS("+init=epsg:28992")))

# demonstrate axis labels with angle, both sides:
sp = SpatialPoints(rbind(c(-101,9), c(-101,55), c(-19,9), c(-19,55)), CRS("+init=epsg:4326"))
laea = CRS("+proj=laea +lat_0=30 +lon_0=-40")
sp.l = spTransform(sp, laea)
plot(sp.l, expandBB = c(0, 0.05, 0, .05))
gl = gridlines(sp)
plot(spTransform(gl, laea), add = TRUE)
text(labels(gl, laea))
text(labels(gl, laea, side = 3:4), col = 'red')
title("curved text label demo")

# polar:
pts=SpatialPoints(rbind(c(-180,-70),c(0,-70),c(180,-89),c(180,-70)), CRS("+init=epsg:4326"))
gl = gridlines(pts, easts = seq(-180,180,20), ndiscr = 100)
polar = CRS("+init=epsg:3031")
plot(spTransform(pts, polar), expandBB = c(.05,0,.05,0))
lines(spTransform(gl, polar))
l = labels(gl, polar, side = 3)
l$pos = NULL # pos is too simple, use adj:
text(l, adj = c(0.5, -0.5)) 
l = labels(gl, polar, side = 4)
l$srt = 0 # otherwise they end up upside-down
text(l)
title("grid line labels on polar projection, epsg 3031")
\dontrun{
demo(polar) # adds the map of the antarctic
}
}

\keyword{spatial}
