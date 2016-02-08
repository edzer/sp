\name{SpatialPoints}
\alias{SpatialPoints}
\alias{SpatialPointsDataFrame}
\title{ create objects of class SpatialPoints or SpatialPointsDataFrame}
\description{ create objects of class \link{SpatialPoints-class} or 
\link{SpatialPointsDataFrame-class} from
coordinates, and from coordinates and \code{data.frame}s}
\usage{ 
SpatialPoints(coords, proj4string=CRS(as.character(NA)), bbox = NULL)
SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0), 
      proj4string = CRS(as.character(NA)), match.ID, bbox = NULL)
}
\arguments{
\item{coords}{numeric matrix or data.frame with coordinates 
(each row is a point); in case of SpatialPointsDataFrame an object
of class \link{SpatialPoints-class} is also allowed}
\item{proj4string}{ projection string of class \link{CRS-class}}
\item{bbox}{bounding box matrix, usually NULL and constructed from the data, but may be passed through for coercion purposes if clearly needed}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data} 
should equal the number of points in the \code{coords} object}
\item{coords.nrs}{numeric; if present, records the column positions where
in \code{data} the coordinates were taken from (used by \link{coordinates<-})}
\item{match.ID}{ logical or character; if missing, and \code{coords} and \code{data} both have
row names, and their order does not correspond, matching is done by these
row names and a warning is issued; this warning can be suppressed by setting
\code{match.ID} to TRUE.
If TRUE AND coords has non-automatic 
rownames (i.e., coerced to a matrix by \code{as.matrix},
\code{dimnames(coords)[[1]]} is not \code{NULL}), AND \code{data} has
row.names (i.e. is a data.frame), then the \code{SpatialPointsDataFrame}
object is formed by matching the row names of both components, leaving
the order of the coordinates in tact. Checks are done to see whether
both row names are sufficiently unique, and all data are matched.
If FALSE, coordinates and data are simply "glued" together, ignoring row names. If
character: indicates the column in \code{data} with coordinates IDs
to use for matching records. See examples below.  }
}
\value{
\code{SpatialPoints} returns an object of class \code{SpatialPoints};
\code{SpatialPointsDataFrame} returns an object of class \code{SpatialPointsDataFrame};
}
\seealso{ \link{coordinates}, \link{SpatialPoints-class},
\link{SpatialPointsDataFrame-class} }
\examples{
set.seed(1331)
pts = cbind(1:5, 1:5)
dimnames(pts)[[1]] = letters[1:5]
df = data.frame(a = 1:5)
row.names(df) = letters[5:1]

library(sp)
options(warn=1) # show warnings where they occur
SpatialPointsDataFrame(pts, df) # warn
SpatialPointsDataFrame(pts, df, match.ID = TRUE) # don't warn
SpatialPointsDataFrame(pts, df, match.ID = FALSE) # don't warn
df$m = letters[5:1]
SpatialPointsDataFrame(pts, df, match.ID = "m") # don't warn

dimnames(pts)[[1]] = letters[5:1]
SpatialPointsDataFrame(pts, df) # don't warn: ID matching doesn't reorder
}
\keyword{manip}
