\name{elide-methods}
\docType{methods}
\alias{elide-methods}
\alias{elide,SpatialPoints-method}
\alias{elide,SpatialPointsDataFrame-method}
\alias{elide,SpatialLines-method}
\alias{elide,SpatialLinesDataFrame-method}
\alias{elide,SpatialPolygons-method}
\alias{elide,SpatialPolygonsDataFrame-method}
\alias{elide}
\title{Methods for Function elide in Package `maptools'}
\description{
 Methods for function \code{elide} to translate and disguise coordinate placing in the real world. 
}
\section{Methods}{
\describe{

\item{obj = "SpatialPoints"}{elides object}

\item{obj = "SpatialPointsDataFrame"}{elides object}

\item{obj = "SpatialLines"}{elides object}

\item{obj = "SpatialLinesDataFrame"}{elides object}

\item{obj = "SpatialPolygons"}{elides object}

\item{obj = "SpatialPolygonsDataFrame"}{elides object}
}}

\usage{
elide(obj, ...)
% (obj, bb=NULL, shift=c(0, 0), reflect=c(FALSE, FALSE), scale=NULL, flip=FALSE)
}

\arguments{
  \item{obj}{object to be elided}
  \item{...}{other arguments:
  \describe{
    \item{bb}{if NULL, uses bounding box of object, otherwise the given bounding box}
    \item{shift}{values to shift the coordinates of the input object; this is made ineffective by the scale argument}
    \item{reflect}{reverse coordinate axes}
    \item{scale}{if NULL, coordinates not scaled; if TRUE, the longer dimension is scaled to lie within [0,1] and aspect maintained; if a scalar, the output range of [0,1] is multiplied by scale}
    \item{flip}{translate coordinates on the main diagonal}
    \item{rotate}{default 0, rotate angle degrees clockwise around center}
    \item{center}{default NULL, if not NULL, the rotation center, numeric of length two}
    \item{unitsq}{logical, default FALSE, if TRUE and scale TRUE, impose unit square bounding box (currently only points)}
}}
}

\value{
The methods return objects of the input class object with elided coordinates; the coordinate reference system is not set. Note that if the input coordinates or centroids are in the data slot data.frame of the input object, they should be removed before the use of these methods, otherwise they will betray the input positions.
}
\note{Rotation code kindly contributed by Don MacQueen}

\examples{
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
data(meuse.riv)
river_polygon <- Polygons(list(Polygon(meuse.riv)), ID="meuse")
rivers <- SpatialPolygons(list(river_polygon))
proj4string(rivers) <- CRS("+init=epsg:28992")
rivers1 <- elide(rivers, reflect=c(TRUE, TRUE), scale=TRUE)
meuse1 <- elide(meuse, bb=bbox(rivers), reflect=c(TRUE, TRUE), scale=TRUE)
opar <- par(mfrow=c(1,2))
plot(rivers, axes=TRUE)
plot(meuse, add=TRUE)
plot(rivers1, axes=TRUE)
plot(meuse1, add=TRUE)
par(opar)
meuse1 <- elide(meuse, shift=c(10000, -10000))
bbox(meuse)
bbox(meuse1)
rivers1 <- elide(rivers, shift=c(10000, -10000))
bbox(rivers)
bbox(rivers1)
meuse1 <- elide(meuse, rotate=-30, center=apply(bbox(meuse), 1, mean))
bbox(meuse)
bbox(meuse1)
plot(meuse1, axes=TRUE)
}

\keyword{methods}
\keyword{spatial}
