\name{SpatialLinesDataFrame-class}
\docType{class}
\alias{SpatialLinesDataFrame-class}
\alias{coerce,SpatialLinesDataFrame,data.frame-method}
\alias{[,SpatialLinesDataFrame-method}
\alias{rbind.SpatialLinesDataFrame}
\alias{coerce,SpatialLinesDataFrame,SpatialPointsDataFrame-method}
\alias{coerce,SpatialLinesDataFrame,SpatialMultiPointsDataFrame-method}
\alias{dim.SpatialLinesDataFrame}
\alias{row.names.SpatialLinesDataFrame}

\title{a class for spatial lines with attributes }
\description{ this class holds data consisting of (sets of lines), where each
set of lines relates to an attribute row in a data.frame }
\section{Objects from the Class}{
can be created by the function \link{SpatialLinesDataFrame}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \link{data.frame} containing the attribute table }
    \item{\code{lines}:}{Object of class \code{"list"}; see \link{SpatialLines-class}}
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"SpatialLines"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialLines"}.
}
\section{Methods}{
Methods defined with class "SpatialLinesDataFrame" in the signature:
  \describe{
    \item{[}{\code{signature(x = "SpatialLinesDataFrame")}: subset rows or
	columns; in case of row subsetting, the line sets are also subsetted; NAs are not permitted in the row index }
    \item{coordinates}{\code{signature(obj = "SpatialLinesDataFrame")}: retrieves
	a list with lists of coordinate matrices }
    \item{show}{\code{signature(object = "SpatialLinesDataFrame")}: print method }
    \item{plot}{\code{signature(x = "SpatialLinesDataFrame")}: plot points}
    \item{lines}{\code{signature(object = "SpatialLinesDataFrame")}: add lines to plot}
    \item{rbind}{\code{signature(object = "SpatialLinesDataFrame")}: 
                rbind-like method}
  }
}
\note{
\code{rbind} for \code{SpatialLinesDataFrame} is only possible for 
objects with unique IDs. If you want to \code{rbind} objects
with duplicated IDs, see\code{\link{spChFIDs}}.
}
\author{ Roger Bivand; Edzer Pebesma }

\seealso{
	\link{SpatialLines-class}
}
\keyword{classes}
