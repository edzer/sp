\name{SpatialPolygons-class}
\docType{class}
\alias{SpatialPolygons-class}
\alias{[,SpatialPolygons-method}
\alias{as.data.frame.SpatialPolygons}
\alias{as.SpatialPolygonsDataFrame.SpatialPolygons}
\alias{coerce,SpatialPolygons,SpatialPolygonsDataFrame-method}
\alias{plot,SpatialPolygons,missing-method}
\alias{summary,SpatialPolygons-method}
\alias{rbind.SpatialPolygons}
\alias{coerce,Polygons,Lines-method}
\alias{coerce,SpatialPolygons,SpatialLines-method}
\alias{coerce,deldir,SpatialPolygons-method}
\alias{row.names.SpatialPolygons}
\alias{get_Polypath}
\alias{set_Polypath}
\alias{get_PolypathRule}
\alias{set_PolypathRule}


\title{Class "SpatialPolygons"}
\description{  class to hold polygon topology (without attributes) }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialPolygons}
}
\section{Slots}{
  \describe{
    \item{\code{polygons}:}{Object of class \code{"list"}; list elements are
	all of class \link{Polygons-class}}
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; integer array
	giving the order in which objects should be plotted }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
Methods defined with class "SpatialPolygons" in the signature:
  \describe{
    \item{[}{\code{signature(obj = "SpatialPolygons")}: select subset of (sets of) polygons; NAs are not permitted in the row index}
	\item{plot}{\code{signature(x = "SpatialPolygons", y = "missing")}: 
	plot polygons in SpatialPolygons object}
	\item{summary}{\code{signature(object = "SpatialPolygons")}: summarize object}
	\item{rbind}{\code{signature(object = "SpatialPolygons")}: rbind-like method}
  }
}

\section{plot method arguments}{
The \code{plot} method for spatial polygons takes the following arguments:
\describe{
  \item{x}{a SpatialPolygons object}
  \item{col}{a vector of colour values}
  \item{border}{default \code{par("fg")}; the colour to draw the border}
  \item{add}{default FALSE; if TRUE, add to existing plot}
  \item{xlim, ylim}{default NULL; ranges for the plotted \sQuote{x} and \sQuote{y} values}
  \item{xpd}{default NULL; controls clipping, see \code{\link[graphics]{par}}}
  \item{density}{default NULL; the density of shading lines, in lines per inch, see \code{\link[graphics]{polygon}}}
  \item{angle}{default 45; the slope of shading lines, given as an angle in degrees (counter-clockwise), see \code{\link[graphics]{polygon}}}
  \item{pbg}{default NULL, set to \code{par("bg")} by default \dQuote{transparent}; the colour to paint holes}
  \item{axes}{default FALSE; draw axes}
  \item{lty}{default \code{par("lty")}; border line type}
  \item{...}{other arguments passed through}
  \item{setParUsrBB}{default FALSE;  see \code{\link{Spatial-class}} for further details}
  \item{usePolypath}{default NULL to set from option value; use \code{\link[graphics]{polypath}} for hole-handling in plot}
  \item{rule}{default NULL to set from option value; character value specifying the path fill mode, see \code{\link[graphics]{polypath}}}
}
The options for \code{usePolypath} and \code{rule} may be retrieved with \code{get_Polypath} (default TRUE on package load) and \code{get_PolypathRule} (default \dQuote{winding} on package load), and set with \code{set_Polypath} and \code{set_PolypathRule}

The class definitions used for polygons in \pkg{sp} do not accord with those of
the simple features specification of the Open Geospatial Consortium. The \pkg{rgeos} package, an interface to Geometry Engine -- Open Source (GEOS), uses this
specification, in which each hole (interior ring) must be associated with
its containing exterior ring. In order to avoid introducing incompatible
changes into the class definition of Polygons objects, a comment has been
added as a single character string to each such object. Here we can trust
the data source to assign the hole status correctly, and use the simple
function \code{\link[rgeos]{createSPComment}} to add such comments to each Polygons member of  
the polygons slot of this SpatialPolygons object. Exterior rings are coded
zero, while interior rings are coded with the 1-based index of the
exterior ring to which they belong. SpatialPolygons objects created by reading using \code{\link[rgdal]{readOGR}} from \pkg{rgdal} have the comments set on input, as OGR also uses SFS.
}

\note{
\code{rbind} calls the function \code{\link{SpatialPolygons}}, where it is 
checked that all IDs are unique. If \code{rbind}-ing \code{SpatialPolygons} without 
unique IDs, it is possible to set the argument \code{makeUniqueIDs = TRUE}, although
it is preferred to change these explicitly with \code{\link{spChFIDs}}.
}
\author{ Roger Bivand }
\seealso{ \code{\link{SpatialPolygons}} }
\examples{
# simple example, from vignette("sp"):
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as(grd, "SpatialPolygons")
plot(polys)
text(coordinates(polys), labels=row.names(polys))
}
\keyword{classes}
