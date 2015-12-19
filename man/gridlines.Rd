\name{gridlines}
\alias{gridlines}
\alias{gridat}

\title{ Create N-S and E-W grid lines over a geographic region }
\description{ Create N-S and E-W grid lines over a geographic region; \code{gridat} permits the construction of points and labels for non-projected grid annotation }
\usage{
gridlines(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]),
 ndiscr = 20) 
gridat(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]),
 offset=0.5, side="WS")
}
\arguments{
  \item{x}{object deriving from class \link{Spatial-class}}
  \item{easts}{numeric; east-west values for vertical lines}
  \item{norths}{numeric; north-south values for horizontal lines}
  \item{ndiscr}{integer; number of points used to discretize the line, could
  be set to 2, unless the grid is (re)projected}
  \item{offset}{offset value to be returned, see \link{text}}
  \item{side}{default \dQuote{WS}, if \dQuote{EN} labels placed on the top and right borders}
}

\value{ \code{gridlines} returns an object of class \link{SpatialLines-class}, with lines as specified;
the return object inherits the projection information of \code{x}; \code{gridat} returns a SpatialPointsDataFrame with points at the west and south ends of the grid lines created by \code{gridlines}, with degree labels}

\author{
  Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}, using example code
  of Roger Bivand.
}

\seealso{ \link[rgdal]{spTransform}; llgridlines in \code{rgdal} (recent versions) for plotting long-lat grid over projected data }
\examples{
data(meuse)
coordinates(meuse) = ~x+y
plot(meuse)
plot(gridlines(meuse), add=TRUE)
title("default gridlines within Meuse bounding box")

if (require(rgdal)) {
proj4string(meuse) <- CRS("+init=epsg:28992")
meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
grd <- gridlines(meuse_ll)
grd_x <- spTransform(grd, CRS("+init=epsg:28992"))
plot(meuse)
plot(grd_x, add=TRUE, lty=2)
grdat_ll <- gridat(meuse_ll)
grdat_x <- spTransform(grdat_ll, CRS("+init=epsg:28992"))
text(coordinates(grdat_x), labels=parse(text=grdat_x$labels),
  pos=grdat_x$pos, offset=grdat_x$offset)
plot(meuse)
plot(grd_x, add=TRUE, lty=2)
grdat_ll <- gridat(meuse_ll, side="EN")
grdat_x <- spTransform(grdat_ll, CRS("+init=epsg:28992"))
text(coordinates(grdat_x), labels=parse(text=grdat_x$labels),
  pos=grdat_x$pos, offset=grdat_x$offset)
}

}

\keyword{spatial}
