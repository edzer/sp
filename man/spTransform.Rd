\name{spTransform}
\alias{spTransform-methods}
\alias{spTransform,Spatial,CRS-method}
\alias{spTransform,Spatial,character-method}
\alias{spTransform,Spatial,ANY-method}
\alias{spTransform}
\title{ spTransform for map projection and datum transformation }
\description{ spTransform for map projection and datum transformation }
\usage{
spTransform(x, CRSobj, ...)
}
\arguments{
\item{x}{ object to be transformed }
\item{CRSobj}{ object of class \link{CRS}, or of class \code{character} in
which case it is converted to \link{CRS}}
\item{...}{further arguments (ignored) }
}
\value{
object with coordinates transformed to the new coordinate reference
system.
}
\note{
Package \code{rgdal} provides the methods doing actual transformation,
see \link[rgdal:spTransform-methods]{spTransform}; when \code{rgdal} cannot be loaded, an
error message follows.
}
\keyword{methods}
\keyword{spatial}
