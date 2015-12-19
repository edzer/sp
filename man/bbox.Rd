\name{bbox-methods}
\docType{methods}
\alias{bbox,Spatial-method}
\alias{bbox,ANY-method}
\alias{bbox,Line-method}
\alias{bbox,Lines-method}
\alias{bbox,Polygon-method}
\alias{bbox,Polygons-method}
\alias{bbox}
\title{ retrieve bbox from spatial data}
\description{
retrieves spatial bounding box from spatial data
}
\usage{
	bbox(obj)
}
\arguments{
\item{obj}{ object deriving from class "Spatial", or one of classes: "Line", "Lines", "Polygon" or "Polygons", or ANY, which requires obj to be an array with at least two columns }
}
\section{Methods}{
\describe{
\item{obj = "Spatial"}{ object deriving from class "Spatial" }
\item{obj = "ANY"}{ an array with at least two columns }
\item{obj = "Line"}{ object deriving from class "Line" }
\item{obj = "Lines"}{ object deriving from class "Lines" }
\item{obj = "Polygon"}{ object deriving from class "Polygon" }
\item{obj = "Polygons"}{ object deriving from class "Polygons" }
}}
\value{ two-column matrix; the first column has the minimum,
the second the maximum values; rows represent the spatial
dimensions}
\keyword{methods}
\examples{
# just 9 points on a grid:
x <- c(1,1,1,2,2,2,3,3,3)
y <- c(1,2,3,1,2,3,1,2,3)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
bbox(S)

# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
bbox(meuse.grid)

}
\keyword{spatial}
