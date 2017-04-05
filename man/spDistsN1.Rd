\name{spDistsN1}
\alias{spDistsN1}
\alias{spDists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Euclidean or Great Circle distance between points}
\description{
  The function returns a vector of distances between a matrix of 2D points, first column longitude, second column latitude, and a single 2D point, using Euclidean or Great Circle distance (WGS84 ellipsoid) methods.
}
\usage{
spDistsN1(pts, pt, longlat = FALSE)
spDists(x, y = x, longlat = FALSE, segments = FALSE, diagonal = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pts}{A matrix of 2D points, first column x/longitude, second column y/latitude, or a SpatialPoints or SpatialPointsDataFrame object}
  \item{pt}{A single 2D point, first value x/longitude, second value y/latitude, or a SpatialPoints or SpatialPointsDataFrame object with one point only}
  \item{x}{A matrix of n-D points with row denoting points, first column x/longitude, second column y/latitude, or a Spatial object that has a \link{coordinates} method}
  \item{y}{A matrix of n-D points with row denoting points, first column x/longitude, second column y/latitude, or a Spatial object that has a \link{coordinates} method}
  \item{longlat}{logical;
  if FALSE, Euclidean distance,
  if TRUE Great Circle (WGS84 ellipsoid) distance; 
  if \code{x} is a Spatial object, longlat should not be specified but will be derived 
  from \link{is.projected}\code{(x)} }
  \item{segments}{logical; if \code{TRUE}, \code{y} must be missing; the vector of distances
  between consecutive points in \code{x} is returned. }
  \item{diagonal}{logical; if \code{TRUE}, \code{y} must be given and have the same number of
  points as \code{x}; the vector with distances between points with identical index is returned. }
}

\value{
\code{spDistsN1} returns a numeric vector of distances in the metric of the points if longlat=FALSE, or in kilometers if longlat=TRUE.

\code{spDists} returns a full matrix of distances in the metric of the points if longlat=FALSE, or in kilometers if longlat=TRUE; it uses \code{spDistsN1} in case points are two-dimensional. In case of \code{spDists(x,x)}, it will compute 
all n x n distances, not the sufficient n x (n-1).
}

\note{The function can also be used to find a local kilometer equivalent to a plot scaled in decimal degrees in order to draw a scale bar.  }
\references{\code{http://www.abecedarical.com/javascript/script_greatcircle.html}}
\author{Roger Bivand, Edzer Pebesma}


\seealso{\code{\link{is.projected}}}
\examples{
ll <- matrix(c(5, 6, 60, 60), ncol=2)
km <- spDistsN1(ll, ll[1,], longlat=TRUE)
zapsmall(km)
utm32 <- matrix(c(276.9799, 332.7052, 6658.1572, 6655.2055), ncol=2)
spDistsN1(utm32, utm32[1,])
dg <- spDistsN1(ll, ll[1,])
dg
dg[2]/km[2]
data(meuse)
coordinates(meuse) <- c("x", "y")
res <- spDistsN1(meuse, meuse[1,])
summary(res)

p1 = SpatialPoints(cbind(1:3, 1:3))
spDists(p1)
spDists(p1, p1)
spDists(p1, p1, diagonal = TRUE)
try(spDists(p1, p1, segments = TRUE))
spDists(p1, segments = TRUE)
p2 = SpatialPoints(cbind(5:2, 2:5))
spDists(p1, p2)
try(spDists(p1, p2, diagonal = TRUE)) # fails
try(spDists(p1, p2, segments = TRUE)) # fails

# longlat points:
proj4string(p1) = "+proj=longlat +ellps=WGS84"
proj4string(p2) = "+proj=longlat +ellps=WGS84"
is.projected(p1)
is.projected(p2)
spDists(p1)
spDists(p1, p1)
spDists(p1, p1, diagonal = TRUE)
spDists(p1, p2)
try(spDists(p1, p2, diagonal = TRUE)) # fails
spDists(p1, p2[1:length(p1),], diagonal = TRUE)
spDists(p1, segments = TRUE)
spDists(p1[0],p2[0],diagonal=TRUE)
spDists(p1[0])

p1 = SpatialPoints(cbind(1:3, 1:3, 1:3))
spDists(p1)
spDists(p1, p1)
try(spDists(p1, p1, diagonal = TRUE))
try(spDists(p1, p1, segments = TRUE))
try(spDists(p1, segments = TRUE))
p2 = SpatialPoints(cbind(5:2, 2:5, 3:6))
spDists(p1, p2)
try(spDists(p1, p2, diagonal = TRUE)) # fails
try(spDists(p1, p2, segments = TRUE)) # fails

}
\keyword{spatial}
