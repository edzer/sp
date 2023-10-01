\name{as.SpatialPolygons.PolygonsList}
\alias{as.SpatialPolygons.PolygonsList}
%\alias{as.SpatialPolygons.Shapes}
%\alias{as.SpatialPolygons.map}
%\alias{as.SpatialPolygons.pal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Making SpatialPolygons objects}
\description{
  This function is used in making SpatialPolygons objects from other formats.
}
\usage{
as.SpatialPolygons.PolygonsList(Srl, proj4string=CRS(as.character(NA)))
%as.SpatialPolygons.Shapes(shapes, IDs, proj4string=CRS(as.character(NA)))
%as.SpatialPolygons.map(map, IDs, proj4string=CRS(as.character(NA)))
%as.SpatialPolygons.pal(arc, pal, IDs, dropPoly1=TRUE, proj4string=CRS(as.character(NA)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Srl}{A list of Polygons objects}
%  \item{shapes}{The Shapes component of a Map object as read by \code{read.shape}}
%  \item{IDs}{Unique character ID values for each output Polygons object; the input IDs can be an integer or character vector with duplicates, where the duplicates will be combined as a single output Polygons object}
  \item{proj4string}{Object of class \code{"CRS"};
	holding a valid proj4 string}
%  \item{map}{Object returned by \code{map} containing polygon boundaries}
%  \item{arc}{Object returned by \code{get.arcdata}}
%  \item{pal}{Object returned by \code{get.paldata}}
%  \item{dropPoly1}{Should the first polygon in the AVC or e00 data be dropped; the first polygon is typically the compound boundary of the whole dataset, and can be detected by looking at the relative lengths of the list components in the second component of pal, which are the numbers of arcs making up the boundary of each polygon}
}

\value{
  The functions return a SpatialPolygons object
}

\author{ Roger Bivand }

\examples{
grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as.SpatialPolygons.GridTopology(grd)
plot(polys)
text(coordinates(polys), labels=sapply(slot(polys, "polygons"), function(i) slot(i, "ID")), cex=0.6)
}
\keyword{spatial}
