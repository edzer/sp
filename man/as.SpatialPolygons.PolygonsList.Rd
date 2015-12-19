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
%\dontrun{
%library(maptools)
%nc.shp <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
%}
%data(ncshp)
%nc1 <- as.SpatialPolygons.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
%plot(nc1)
%invisible(text(coordinates(nc1), labels=getSpPPolygonsIDSlots(nc1), cex=0.6))
%\dontrun{
%library(maps)
%ncmap <- map("county", "north carolina", fill=TRUE, col="transparent", plot=FALSE)
%}
%data(ncmap)
%IDs <- sapply(strsplit(ncmap$names, "[,:]"), function(x) x[2])
%nc2 <- as.SpatialPolygons.map(ncmap, IDs)
%plot(nc2)
%invisible(text(coordinates(nc2), labels=getSpPPolygonsIDSlots(nc2), cex=0.6))
%\dontrun{
%library(RArcInfo)
%fl <- "http://www.census.gov/geo/cob/bdy/co/co90e00/co37_d90_e00.zip"
%download.file(fl, "co37_d90_e00.zip")
%e00 <- zip.file.extract("co37_d90.e00", "co37_d90_e00.zip")
%e00toavc(e00, "nc")
%arc <- get.arcdata(".", "nc")
%pal <- get.paldata(".", "nc")
%pat <- get.tabledata("info", "NC.PAT")
%}
%data(co37_d90_arc)
%data(co37_d90_pal)
%sapply(pal[[2]], function(x) length(x[[1]]))
%data(co37_d90_pat)
%IDs <- paste(pat$ST[-1], pat$CO[-1], sep="")
%nc3 <- as.SpatialPolygons.pal(arc, pal, IDs=IDs)
%plot(nc3)
%invisible(text(coordinates(nc3), labels=sapply(slot(nc3, "polygons"), function(i) slot(i, "ID")), cex=0.6))
grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as.SpatialPolygons.GridTopology(grd)
plot(polys)
text(coordinates(polys), labels=sapply(slot(polys, "polygons"), function(i) slot(i, "ID")), cex=0.6)
}
\keyword{spatial}
