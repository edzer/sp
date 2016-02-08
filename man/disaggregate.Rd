\name{disaggregate-methods}
\docType{methods}
\alias{disaggregate}
\alias{disaggregate-methods}
\alias{disaggregate,SpatialPolygons-method}
\alias{disaggregate,SpatialPolygonsDataFrame-method}
\alias{disaggregate,SpatialLines-method}
\alias{disaggregate,SpatialLinesDataFrame-method}
\title{ disaggregate SpatialLines, SpatialLinesDataFrame, 
SpatialPolygons, or SpatialPolygonsDataFrame objects }
\description{ disaggregate SpatialLines, SpatialLinesDataFrame, 
SpatialPolygons, or SpatialPolygonsDataFrame objects }
\usage{ disaggregate(x, ...) }
\arguments{
\item{x}{object of class \link{SpatialLines} or \link{SpatialPolygons}}
\item{...}{ignored}
}
\value{ 
object of class \link{SpatialLines} or \link{SpatialPolygons},
where groups of \link{Line} or \link{Polygon} are disaggregated to
one \link{Line} per \link{Lines}, or one \link{Polygon}
per \link{Polygons}, respectively.
}

\author{ Robert Hijmans, Edzer Pebesma }
\examples{
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)), hole = FALSE)
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)), hole = FALSE)
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)), hole = FALSE)
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1, Sr2), "s1/2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
sp = SpatialPolygons(list(Srs1,Srs3), 1:2)
length(sp) ## [1] 2
length(disaggregate(sp)) ## [1] 3 

l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
sl = SpatialLines(list(S1,S2))
length(sl)
length(disaggregate(sl))
}
\keyword{methods}
