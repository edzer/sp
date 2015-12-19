\name{Polygons-class}
\docType{class}
\alias{Polygons-class}

\title{Class "Polygons"}
\description{ Collection of objects of class \code{"Polygon"} }
\section{Objects from the Class}{
Objects can be created by calls to the function \code{Polygons}
}
\section{Slots}{
  \describe{
    \item{\code{Polygons}:}{Object of class \code{"list"}; list with objects
	of class \link{Polygon-class} }
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; order in which
	the Polygon objects should be plotted, currently by order of decreasing size }
    \item{\code{labpt}:}{Object of class \code{"numeric"}; pair of x, y coordinates giving a label point, the label point of the largest polygon component  }
    \item{\code{ID}:}{Object of class \code{"character"}; unique identifier string  }
    \item{\code{area}:}{Object of class \code{"numeric"}; the gross total planar area of the Polygon list but not double-counting holes (changed from 0.9-58 - islands are summed, holes are ignored rather than subtracted); these values are used to make sure that polygons of a smaller area are plotted after polygons of a larger area, does not respect projection as objects of this class have no projection defined }
  }
}
\section{Methods}{
No methods defined with class "Polygons" in the signature.
}
\author{ Roger Bivand }
\note{ By default, single polygons (where Polygons is a list of length one) are not expected to be holes, but in multiple polygons, hole definitions for member polygons can be set. Polygon objects belonging to an Polygons object should either not overlap one-other, or should be fully included (as lakes or islands in lakes). They should not be self-intersecting. Checking of hole FALSE/TRUE status for Polygons objects is included in the maptools package using functions in the rgeos package, function checkPolygonsHoles(). }

\keyword{classes}
