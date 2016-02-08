\name{Polygon-class}
\docType{class}
\alias{Polygon-class}

\title{Class "Polygon" }
\description{ class for spatial polygon }
\section{Objects from the Class}{
Objects can be created by calls to the function \code{Polygon}
}
\section{Slots}{
  \describe{
    \item{\code{ringDir}:}{Object of class \code{"integer"}; the ring direction of the ring (polygon) coordinates, holes are expected to be anti-clockwise }
    \item{\code{labpt}:}{Object of class \code{"numeric"}; an x, y coordinate pair forming the label point of the polygon }
    \item{\code{area}:}{Object of class \code{"numeric"}; the planar area of the polygon, does not respect projection as objects of this class have no projection defined }
    \item{\code{hole}:}{Object of class \code{"logical"}; does the polygon seem to be a hole }
    \item{\code{coords}:}{Object of class \code{"matrix"}; coordinates of the polygon;
	first point should equal the last point }
  }
}
\section{Extends}{
Class \code{"Line"}, directly.
}
\section{Methods}{
No methods defined with class "Polygon" in the signature.
}
\author{ Roger Bivand }

\seealso{\link{Polygons-class}, \link{SpatialPolygons-class}}

\keyword{classes}
