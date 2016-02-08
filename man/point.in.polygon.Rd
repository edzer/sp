\name{point.in.polygon}
\alias{point.in.polygon}
\title{ do point(s) fall in a given polygon? }
\description{
verifies for one or more points whether they fall in a given polygon
}
\usage{
point.in.polygon(point.x, point.y, pol.x, pol.y, mode.checked=FALSE)
}
\arguments{
\item{point.x}{numerical array of x-coordinates of points}
\item{point.y}{numerical array of y-coordinates of points}
\item{pol.x}{numerical array of x-coordinates of polygon}
\item{pol.y}{numerical array of y-coordinates of polygon}
\item{mode.checked}{default FALSE, used internally to save time when all the other argument are known to be of storage mode double}
}
\value{ integer array; values are:
0: point is strictly exterior to pol;
1: point is strictly interior to pol;
2: point lies on the relative interior of an edge of pol;
3: point is a vertex of pol.
}
\references{ 
Uses the C function InPoly().  InPoly is Copyright (c) 1998 by
Joseph O'Rourke.  It may be freely redistributed in its entirety
provided that this copyright notice is not removed.
}
\examples{
# open polygon:
point.in.polygon(1:10,1:10,c(3,5,5,3),c(3,3,5,5))
# closed polygon:
point.in.polygon(1:10,rep(4,10),c(3,5,5,3,3),c(3,3,5,5,3))
}
\keyword{manip}
