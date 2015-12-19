\name{gridIndex2nb}
\alias{gridIndex2nb}
\title{create neighbourhood (nb) object from grid geometry}
\description{create neighbourhood (nb) object from grid geometry}
\usage{ gridIndex2nb(obj, maxdist = sqrt(2), fullMat = TRUE, ...) }
\arguments{
\item{obj}{object of class \link{SpatialGrid} or \link{SpatialPixels}}
\item{maxdist}{ maximum distance to be considered (inclusive), expressed in number
of grid cell (sqrt(2) results in queen neighbours) }
\item{fullMat}{ use \link{dist} to compute distances from grid (row/col) indices;
FALSE avoids forming the full distance matrix, at a large performance cost }
\item{...}{ arguments passed on to \link{dist}}
}
\value{Object of class nb, which is a list.

The nb object follows the convention of nb objects in package spdep;
it is a list with each list element corresponding to a grid cell or
pixel; the list element contains the indices of neighbours defined
as cells less than maxdist away, measured in cell unit (N/S/E/W
neighbour has distance 1). 
}

\note{ Unequal grid cell size is ignored; grid cell row/col indices
are taken to be the coordinates from which distances are computed.

}
\seealso{plot.nb in package spdep}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
%\examples{}
\keyword{manip}
