\name{degAxis}
\alias{degAxis}
\title{ axis with degrees }
\description{
draw axes on a plot using degree symbols in numbers
}
\usage{
degAxis(side, at, labels, ...)
}
\arguments{
\item{side}{integer; see \link{axis}}
\item{at}{numeric; if missing, \link{axTicks} is called for nice values; see \link{axis}}
\item{labels}{character; if omitted labels are constructed with degree symbols, ending
in N/S/E/W; in case of negative degrees, sign is reversed and S or W is added; see \link{axis}}
\item{...}{passed to the actual \link{axis} call}
}
\value{ axis is plotted on current graph }
\note{decimal degrees are used if variation is small, instead of minutes and seconds}
\examples{
xy = cbind(x = 2 * runif(100) - 1, y = 2 * runif(100) - 1)
plot(SpatialPoints(xy, proj4string = CRS("+proj=longlat +ellps=WGS84")),xlim=c(-1,1),ylim=c(-1,1))
degAxis(1)
degAxis(2, at = c(-1,-0.5,0,0.5,1))
#
}
\keyword{dplot}
