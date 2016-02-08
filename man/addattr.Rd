\name{addAttrToGeom-methods}
\docType{methods}
\alias{addAttrToGeom}
\alias{addAttrToGeom-methods}
\alias{addAttrToGeom,SpatialPoints,data.frame-method}
\alias{addAttrToGeom,SpatialMultiPoints,data.frame-method}
\alias{addAttrToGeom,SpatialGrid,data.frame-method}
\alias{addAttrToGeom,SpatialPixels,data.frame-method}
\alias{addAttrToGeom,SpatialPolygons,data.frame-method}
\alias{addAttrToGeom,SpatialLines,data.frame-method}

\title{ constructs SpatialXxxDataFrame from geometry and attributes}
\description{ constructs SpatialXxxDataFrame from geometry and attributes}
\usage{
addAttrToGeom(x, y, match.ID, ...)
}
\arguments{
\item{x}{geometry (locations) of the queries}
\item{y}{data.frame object with attributes}
\item{match.ID}{ logical; if TRUE, the IDs of the geometry and of the
data.frame are matched (possibly swapping records), and an error occurs
when some IDs do not match }
\item{...}{(optional) arguments passed to the constructor functions }
}
\value{ an object of class XxxDataFrame, where Xxx is the class of x}

\section{Methods}{
\describe{
\item{x = "SpatialPoints", y = "data.frame"}{ }
\item{x = "SpatialPixels", y = "data.frame"}{ }
\item{x = "SpatialGrid", y = "data.frame"}{ }
\item{x = "SpatialLines", y = "data.frame"}{ }
\item{x = "SpatialPolygons", y = "data.frame"}{ }
}
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}
\seealso{\link{over}}
\keyword{methods}
