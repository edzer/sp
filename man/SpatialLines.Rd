\name{SpatialLines}
\alias{SpatialLines}
\alias{SpatialLinesDataFrame}
\alias{as.SpatialLines.SLDF}
\alias{getSpatialLinesMidPoints}
\alias{LineLength}
\alias{LinesLength}
\alias{SpatialLinesLengths}
\title{ create objects of class SpatialLines or SpatialLinesDataFrame}
\description{ create objects of class \code{SpatialLines} or 
\code{SpatialLinesDataFrame} from
lists of \code{Lines} objects and data.frames; extract list od Lines from a SpatialLines object}
\usage{ 
SpatialLines(LinesList, proj4string = CRS(as.character(NA)))
SpatialLinesDataFrame(sl, data, match.ID = TRUE)
as.SpatialLines.SLDF(SLDF)
getSpatialLinesMidPoints(SL)
LineLength(cc, longlat = FALSE, sum = TRUE)
LinesLength(Ls, longlat = FALSE)
SpatialLinesLengths(SL, longlat)
}
\arguments{
\item{LinesList}{ list with objects of class \link{Lines-class}}
\item{proj4string}{Object of class \code{"CRS"}; holding a valid proj4 string}

\item{sl, SL}{ object of class \link{SpatialLines-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data}
should equal the number of \code{Lines} elements in \code{sl}}
\item{match.ID}{logical: (default TRUE): match SpatialLines member
Lines ID slot values with data.frame row names, and re-order the
data frame rows if necessary; if character: indicates the column
in \code{data} with Lines IDs to match}
\item{SLDF}{SpatialLinesDataFrame object}
\item{Ls}{Object of class \code{Lines}}
\item{cc}{Object of class \code{Line}, or two-column matrix with points}
\item{longlat}{if FALSE, Euclidean distance, if TRUE Great Circle distance in kilometers}
\item{sum}{logical; if \code{TRUE} return scalar length of sum of 
segments in Line, if \code{FALSE} return vector with segment lengths}
}
\value{
\code{SpatialLines} returns object of class \code{SpatialLines};
\code{SpatialLinesDataFrame} returns object of class \code{SpatialLinesDataFrame}
\code{getSpatialLinesMidPoints} returns an object of class \code{SpatialPoints},each point containing the (weighted) mean of the lines elements; weighted in the sense that mean is called twice.
}
\seealso{ \link{SpatialLines-class} }

\keyword{manip}
