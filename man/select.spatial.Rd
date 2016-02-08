\name{select.spatial}
\alias{select.spatial}
\title{ select points spatially }
\description{
select a number of points by digitizing the area they fall in
}
\usage{
select.spatial(data, digitize = TRUE, pch = "+", rownames = FALSE)
}
\arguments{
\item{data}{data object of class, or extending \code{SpatialPoints};
this object knows about its x and y coordinate }
\item{digitize}{ logical; if TRUE, points in a digitized polygon
are selected; if FALSE, points identified by mouse clicks are selected }
\item{pch}{plotting character used for points}
\item{rownames}{logical; if FALSE, row (coordinate) numbers are returned;
if TRUE and data contains a data.frame part, row.names for selected points
in the data.frame are returned. }
}
\value{ if rownames == FALSE, array with either indexes (row numbers)
of points inside the digitized polygon; if rownames == TRUE, character
array with corresponding row names in the data.frame part }
\seealso{\link{point.in.polygon}, \link{locator}, 
\link{SpatialPoints-class}, \link{SpatialPointsDataFrame-class}
}
\examples{
data(meuse)
## the following command requires user interaction: left mouse
## selects points, right mouse ends digitizing
data(meuse)
coordinates(meuse) = c("x", "y")
# select.spatial(meuse)
}
\keyword{models}
