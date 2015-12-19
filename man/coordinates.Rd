\name{coordinates}
\alias{coordinates}
\alias{coordinates<-}

\title{ set spatial coordinates to create a Spatial object, or retrieve
spatial coordinates from a Spatial object}
\description{ set spatial coordinates to create a Spatial object, or retrieve
spatial coordinates from a Spatial object}
\usage{
	coordinates(obj, ...)
	coordinates(object) <- value
}
\arguments{
\item{obj}{ object deriving from class "Spatial" }
\item{object}{ object of class "data.frame" }
\item{value}{ spatial coordinates; either a matrix, list, or data
frame with numeric data, or column names, column number or a
reference: a formula (in the form of e.g. \code{~x+y}), column numbers
(e.g. \code{c(1,2)}) or column names (e.g. \code{c("x","y")}) specifying
which columns in \code{object} are the spatial coordinates. If the coordinates
are part of object, giving the reference does not duplicate them, giving
their value does duplicate them in the resulting structure.
}
\item{...}{additional arguments that may be used by particular methods}
}
\value{ usually an object of class SpatialPointsDataFrame; if the coordinates
set cover the full set of variables in object, an object of class SpatialPoints
is returned }
\examples{
# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
class(meuse.grid)
bbox(meuse.grid)

data(meuse)
meuse.xy = meuse[c("x", "y")]
coordinates(meuse.xy) <- ~x+y
class(meuse.xy)

}
\keyword{manip}
