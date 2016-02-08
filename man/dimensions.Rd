\name{dimensions-methods}
\docType{methods}
\alias{dimensions,Spatial-method}
\alias{dimensions}

\title{ retrieve spatial dimensions from spatial data}
\description{
retrieves spatial dimensions box from spatial data
}
\usage{
	dimensions(obj)
}
\arguments{
\item{obj}{ object deriving from class "Spatial" }
}
\section{Methods}{
\describe{
\item{obj = "Spatial"}{ object deriviving from class "Spatial" }
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
dimensions(S)

# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
dimensions(meuse.grid)

}
\keyword{methods}
