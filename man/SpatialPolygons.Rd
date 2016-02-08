\name{SpatialPolygons}
\alias{SpatialPolygons}
\alias{SpatialPolygonsDataFrame}
\alias{Polygon}
\alias{Polygons}
\alias{getSpatialPolygonsLabelPoints}
\title{ create objects of class SpatialPolygons or SpatialPolygonsDataFrame}
\description{ create objects of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} from
lists of \code{Polygons} objects and \code{data.frame}s}
\usage{ 
Polygon(coords, hole=as.logical(NA))
Polygons(srl, ID)
SpatialPolygons(Srl, pO, proj4string=CRS(as.character(NA)))
SpatialPolygonsDataFrame(Sr, data, match.ID = TRUE)
getSpatialPolygonsLabelPoints(SP)

}
\arguments{
\item{coords}{2-column numeric matrix with coordinates; first point (row) should
equal last coordinates (row); if the hole argument is not given, the status of the polygon as a hole or an island will be taken from the ring direction, with clockwise meaning island, and counter-clockwise meaning hole}
\item{hole}{logical value for setting polygon as hole or not; if the hole argument is not given, the status of the polygon as a hole or an island will be taken from the ring direction, with clockwise meaning island, and counter-clockwise meaning hole}
\item{proj4string}{ projection string of class \link{CRS-class}}
\item{srl}{list with \link{Polygon-class} objects}
\item{ID}{character vector of length one with identifier}
\item{Srl}{ list with objects of class \link{Polygons-class}}
\item{pO}{ integer vector; plotting order; if missing in reverse order of Polygons area }
\item{Sr}{ object of class \link{SpatialPolygons-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data} should equal the number of \link{Polygons-class} objects in \code{Sr}}
\item{match.ID}{logical: (default TRUE): match SpatialPolygons
member Polygons ID slot values with data frame row names, and
re-order the data frame rows if necessary. If character: indicates
the column in \code{data} with Polygons IDs to match}
\item{SP}{object of class \link{SpatialPolygons-class}}
} 

\details{
In \code{Polygon}, if the hole argument is not given, the status of the polygon as a hole or an island will be taken from the ring direction, with clockwise meaning island, and counter-clockwise meaning hole. In \code{Polygons}, if all of the member Polygon objects are holes, the largest by area will be converted to island status. Until 2010-04-17, version 0.9-61, the area of this converted object was erroneously left at its hole value of zero. Thanks to Patrick Giraudoux for spotting the bug. 

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

\value{
\code{Polygon} returns an object of class \code{Polygon};
\code{Polygons} returns an object of class \code{Polygons};
\code{SpatialPolygons} returns object of class \code{SpatialPolygons};
\code{SpatialPolygonsDataFrame} returns object of class \code{SpatialPolygonsDataFrame}
\code{getSpatialPolygonsLabelPoints} returns an object of class
\code{SpatialPoints} with label points.
}
\seealso{ \link{SpatialPolygons-class}, \link{SpatialPolygonsDataFrame-class} }

\keyword{manip}
