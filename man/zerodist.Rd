\name{zerodist}
\alias{zerodist}
\alias{zerodist2}
\alias{remove.duplicates}
\title{ find point pairs with equal spatial coordinates }
\description{ find point pairs with equal spatial coordinates }
\usage{ 
zerodist(obj, zero = 0.0, unique.ID = FALSE, memcmp = TRUE) 
zerodist2(obj1, obj2, zero = 0.0, memcmp = TRUE) 
remove.duplicates(obj, zero = 0.0, remove.second = TRUE, memcmp = TRUE)
}
\arguments{
\item{obj}{ object of, or extending, class \link{SpatialPoints} }
\item{obj1}{ object of, or extending, class \link{SpatialPoints} }
\item{obj2}{ object of, or extending, class \link{SpatialPoints} }
\item{zero}{ distance values less than or equal to this threshold value are
considered to have zero distance (default 0.0);
units are those of the coordinates for projected data or unknown projection,
or km if coordinates are defined to be longitute/latitude }
\item{unique.ID}{logical; if TRUE, return an ID (integer) for each point 
that is different only when two points do not share the same location }
\item{memcmp}{use \code{memcmp} to find exactly equal coordinates; see NOTE}
\item{remove.second}{logical; if TRUE, the second of each pair of duplicate
points is removed, if FALSE remove the first}
}
\value{\code{zerodist} and \code{zerodist2} return a two-column matrix
with in each row pairs of row numbers with identical coordinates;
a matrix with zero rows is returned if no such pairs are found. For
\code{zerodist}, row number pairs refer to row pairs in \code{obj}. For
\code{zerodist2}, row number pairs refer to rows in \code{obj} and
\code{obj2}, respectively. \code{remove.duplicates} removes duplicate
observations if present, and else returns \code{obj}. }

\note{ When using kriging, duplicate observations sharing identical
spatial locations result in singular covariance matrices.
This function may help identify and remove spatial duplices.
The full matrix with all pair-wise distances is not stored; the
double loop is done at the C level.  

When \code{unique.ID=TRUE} is used, an integer index is returned. sp
1.0-14 returned the highest index, sp 1.0-15 and later return the
lowest index.

When \code{zero} is 0.0 and \code{memcmp} is not \code{FALSE},
\code{zerodist} uses \code{memcmp} to evaluate exact equality of
coordinates; there may be cases where this results in a different
evaluation compared to doing the double arithmetic of computing
distances.

}

\examples{
data(meuse)
summary(meuse)
# pick 10 rows
n <- 10
ran10 <- sample(nrow(meuse), size = n, replace = TRUE)
meusedup <- rbind(meuse, meuse[ran10, ])
coordinates(meusedup) <- c("x", "y")
zd <- zerodist(meusedup)
sum(abs(zd[1:n,1] - sort(ran10))) # 0!
# remove the duplicate rows:
meusedup2 <- meusedup[-zd[,2], ]
summary(meusedup2)
meusedup3 <- subset(meusedup, !(1:nrow(meusedup) \%in\% zd[,2]))
summary(meusedup3)
coordinates(meuse) <- c("x", "y")
zerodist2(meuse, meuse[c(10:33,1,10),])
}
\keyword{dplot}
