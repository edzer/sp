\name{SpatialMultiPoints}
\alias{SpatialMultiPoints}
\alias{SpatialMultiPointsDataFrame}
\title{ create objects of class SpatialMultiPoints or SpatialMultiPointsDataFrame}
\description{ create objects of class \link{SpatialMultiPoints-class} or 
\link{SpatialMultiPointsDataFrame-class} from
coordinates, and from coordinates and \code{data.frame}s}
\usage{ 
SpatialMultiPoints(coords, proj4string=CRS(as.character(NA)), bbox = NULL)
SpatialMultiPointsDataFrame(coords, data,
      proj4string = CRS(as.character(NA)), match.ID, bbox = NULL)
}
\arguments{
\item{coords}{list with in each element a numeric matrix or data.frame with coordinates 
(each row representing a point); in case of SpatialMultiPointsDataFrame an object
of class \link{SpatialMultiPoints-class} is also allowed}
\item{proj4string}{ projection string of class \link{CRS-class}}
\item{bbox}{bounding box matrix, usually NULL and constructed from the data, but may be passed through for coercion purposes if clearly needed}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data} 
should equal the number of points in the \code{coords} object}
\item{match.ID}{ logical or character; if missing, and \code{coords} and \code{data} both have
row names, and their order does not correspond, matching is done by these
row names and a warning is issued; this warning can be suppressed by setting
\code{match.ID} to TRUE.
If TRUE AND coords has non-automatic 
rownames (i.e., coerced to a matrix by \code{as.matrix},
\code{dimnames(coords)[[1]]} is not \code{NULL}), AND \code{data} has
row.names (i.e. is a data.frame), then the \code{SpatialMultiPointsDataFrame}
object is formed by matching the row names of both components, leaving
the order of the coordinates in tact. Checks are done to see whether
both row names are sufficiently unique, and all data are matched.
If FALSE, coordinates and data are simply "glued" together, ignoring row names. If
character: indicates the column in \code{data} with coordinates IDs
to use for matching records. See examples below.  }
}
\value{
\code{SpatialMultiPoints} returns an object of class \code{SpatialMultiPoints};
\code{SpatialMultiPointsDataFrame} returns an object of class \code{SpatialMultiPointsDataFrame};
}
\seealso{ \link{coordinates}, \link{SpatialMultiPoints-class},
\link{SpatialMultiPointsDataFrame-class} }
\examples{
cl1 = cbind(rnorm(3, 10), rnorm(3, 10))
cl2 = cbind(rnorm(5, 10), rnorm(5,  0))
cl3 = cbind(rnorm(7,  0), rnorm(7, 10))

mp = SpatialMultiPoints(list(cl1, cl2, cl3))
mpx = rbind(mp, mp) # rbind method
plot(mp, col = 2, cex = 1, pch = 1:3)
mp
mp[1:2]

print(mp, asWKT=TRUE, digits=3)

mpdf = SpatialMultiPointsDataFrame(list(cl1, cl2, cl3), data.frame(a = 1:3))
mpdf
mpdfx = rbind(mpdf, mpdf) # rbind method

plot(mpdf, col = mpdf$a, cex = 1:3)
as(mpdf, "data.frame")
mpdf[1:2,]
}
\keyword{manip}
