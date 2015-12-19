\name{meuse.grid}
\alias{meuse.grid}
\docType{data}
\title{ Prediction Grid for Meuse Data Set }
\usage{data(meuse.grid)}
\description{
The \code{meuse.grid} data frame has 3103 rows and 7 columns;
a grid with 40 m x 40 m spacing that covers the Meuse study area (see \link{meuse})
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{a numeric vector; x-coordinate (see \link{meuse}) }
    \item{y}{a numeric vector; y-coordinate (see \link{meuse}) }
	\item{dist}{distance to the Meuse river; obtained by a spread
	(spatial distance) GIS operation, from border of river; normalized to
	$[0,1]$ }
	\item{ffreq}{flooding frequency class, for definitions see this item in \link{meuse}; it is not known how this map was generated}
	\item{part.a}{arbitrary division of the area in two areas, a and b}
	\item{part.b}{see \code{part.a}}
	\item{soil}{soil type, for definitions see this item in \link{meuse}; it is questionable whether these data come from a real soil map, they do not match the published 1:50 000 map}
  }
}
\details{
\code{x} and \code{y} are in RD New, the Dutch topographical map
coordinate system. Roger Bivand projected this to UTM in the
R-Grass interface package.
}
\source{
\url{http://www.gstat.org/}
}
\references{
See the \link{meuse} documentation
}
\examples{
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) = TRUE
spplot(meuse.grid)
}
\keyword{datasets}
