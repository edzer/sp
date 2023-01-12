\name{spTransform}
\alias{spTransform-methods}
\alias{spTransform,Spatial,CRS-method}
\alias{spTransform,Spatial,character-method}
\alias{spTransform,Spatial,ANY-method}
\alias{spTransform}
\alias{get_evolution_status}
\alias{set_evolution_status}

\title{ spTransform for map projection and datum transformation }
\description{ spTransform for map projection and datum transformation }
\usage{
spTransform(x, CRSobj, ...)
set_evolution_status(value)
get_evolution_status()
}
\arguments{
\item{x}{ object to be transformed }
\item{CRSobj}{ object of class \link{CRS}, or of class \code{character} in
which case it is converted to \link{CRS}}
\item{...}{further arguments (ignored) }
\item{value}{evolution status: 0L business as usual, 1L no retiring packages, 2L use sf functions in place of rgdal}
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
