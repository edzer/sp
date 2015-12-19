\name{gridded-methods}
\docType{methods}
\alias{gridded-methods}
\alias{gridparameters}
\alias{fullgrid}
\alias{fullgrid,Spatial-method}
\alias{gridded}
\alias{gridded,Spatial-method}
\alias{fullgrid<-}
\alias{gridded<-}
\alias{fullgrid<-,Spatial,ANY-method}
\alias{fullgrid<-,SpatialGrid,logical-method}
\alias{fullgrid<-,SpatialGridDataFrame,logical-method}
\alias{fullgrid<-,SpatialPixels,logical-method}
\alias{fullgrid<-,SpatialPixelsDataFrame,logical-method}
\alias{gridded<-,SpatialGrid,logical-method}
\alias{gridded<-,SpatialGridDataFrame,logical-method}
\alias{gridded<-,SpatialPixels,logical-method}
\alias{gridded<-,SpatialPixelsDataFrame,logical-method}
\alias{gridded<-,SpatialPoints,list-method}
\alias{gridded<-,SpatialPoints,logical-method}
\alias{gridded<-,SpatialPointsDataFrame,list-method}
\alias{gridded<-,SpatialPointsDataFrame,logical-method}
\alias{gridded<-,data.frame,GridTopology-method}
\alias{gridded<-,data.frame,character-method}
\alias{gridded<-,data.frame,formula-method}

\title{ specify spatial data as being gridded, or find out whether they are }
\description{
returns logical (TRUE or FALSE) telling whether the object is gridded
or not; in assignment promotes a non-gridded structure to a gridded
one, or demotes a gridded structure back to a non-structured one.
}
\usage{
	gridded(obj)
	gridded(obj) <- value
	fullgrid(obj)
	fullgrid(obj) <- value
	gridparameters(obj)
}
\arguments{
\item{obj}{ object deriviving from class "Spatial" (for gridded), or
object of class \link{SpatialGridDataFrame-class} (for fullgrid and gridparameters)}
\item{value}{logical replacement values, TRUE or FALSE}
}
\section{Methods}{
\describe{
\item{obj = "Spatial"}{ object deriviving from class "Spatial" }
}}
\value{ if obj derives from class Spatial, gridded(object) will tell
whether it is has topology on a regular grid; if assigned TRUE, if the
object derives from SpatialPoints and has gridded topology,
grid topology will be added to object, and the class of the object
will be promoted to \link{SpatialGrid-class} or 
\link{SpatialGridDataFrame-class}

\code{fullgrid} returns a logical, telling whether the grid is full
and ordered (i.e., in full matrix form), or whether it is not full
or unordered (i.e. a list of points that happen to lie on a grid. If
assigned, the way the points are stored may be changed. Changing a set
of points to full matrix form and back may change the original order of
the points, and will remove duplicate points if they were present.

\code{gridparameters} returns, if \code{obj} inherits from
SpatialGridDataFrame its grid parameters, else it returns numeric(0).  The
returned value is a data.frame with three columns, named cellcentre.offset
("lower left cell centre coordinates"), cellsize, and cells.dim (cell
dimension); the rows correspond to the spatial dimensions.

}
\keyword{methods}
\examples{
# just 9 points on a grid:
x <- c(1,1,1,2,2,2,3,3,3)
y <- c(1,2,3,1,2,3,1,2,3)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
class(S)
plot(S)
gridded(S) <- TRUE
gridded(S)
class(S)
summary(S)
plot(S)
gridded(S) <- FALSE
gridded(S)
class(S)

# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
plot(meuse.grid) # not much good
summary(meuse.grid)

}
\keyword{spatial}
