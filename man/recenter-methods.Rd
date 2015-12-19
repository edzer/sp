\name{recenter-methods}
\docType{methods}
\alias{recenter-methods}
\alias{recenter}
\alias{recenter,SpatialPolygons-method}
\alias{recenter,Polygons-method}
\alias{recenter,Polygon-method}
\alias{recenter,SpatialLines-method}
\alias{recenter,Lines-method}
\alias{recenter,Line-method}
\title{Methods for Function recenter in Package `sp'}
\description{
 Methods for function \code{recenter} in package \pkg{sp} to shift or re-center geographical coordinates for a Pacific view. All longitudes < 0 are added to 360, to avoid for instance parts of Alaska being represented on the far left and right of a plot because they have values straddling 180 degrees. In general, using a projected coordinate reference system is to be prefered, but this method permits a geographical coordinate reference system to be used. This idea was suggested by Greg Snow, and corresponds to the two world representations in the \pkg{maps} package. 
}
\section{Methods}{
\describe{

\item{obj = "SpatialPolygons"}{recenter a SpatialPolygons object}

\item{obj = "Polygons"}{recenter a Polygons object }

\item{obj = "Polygon"}{recenter an Polygon object }

\item{obj = "SpatialLines"}{recenter a SpatialLines object }

\item{obj = "Lines"}{recenter a Lines object }

\item{obj = "Line"}{recenter an Line object }
}}

\examples{
crds <- matrix(c(179, -179, -179, 179, 50, 50, 52, 52), ncol=2)
SL <- SpatialLines(list(Lines(list(Line(crds)), "1")),
 CRS("+proj=longlat +ellps=WGS84"))
bbox(SL)
SLr <- recenter(SL)
bbox(SLr)
rcrds <- rbind(crds, crds[1,])
SpP <- SpatialPolygons(list(Polygons(list(Polygon(rcrds)), ID="r1")),
 proj4string=CRS("+proj=longlat +ellps=WGS84"))
bbox(SpP)
SpPr <- recenter(SpP)
bbox(SpPr)
opar <- par(mfrow=c(1,2))
plot(SpP)
plot(SpPr)
par(opar)
crds <- matrix(c(-1, 1, 1, -1, 50, 50, 52, 52), ncol=2)
SL <- SpatialLines(list(Lines(list(Line(crds)), "1")),
 CRS("+proj=longlat +ellps=WGS84"))
bbox(SL)
SLr <- recenter(SL)
bbox(SLr)
rcrds <- rbind(crds, crds[1,])
SpP <- SpatialPolygons(list(Polygons(list(Polygon(rcrds)), ID="r1")),
 proj4string=CRS("+proj=longlat +ellps=WGS84"))
bbox(SpP)
SpPr <- recenter(SpP)
bbox(SpPr)
opar <- par(mfrow=c(1,2))
plot(SpP)
plot(SpPr)
par(opar)
}

\keyword{methods}

