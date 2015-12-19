\name{SpatialPoints-class}
\docType{class}
\alias{SpatialPoints-class}
\alias{[,SpatialPoints-method}
\alias{coerce,SpatialPoints,matrix-method}
\alias{coerce,SpatialPoints,data.frame-method} % here???
\alias{coerce,SpatialPoints,SpatialPixels-method} % here???
\alias{coerce,SpatialPoints,Line-method}
\alias{coerce,SpatialPoints,Lines-method}
\alias{coerce,SpatialPoints,SpatialLines-method}
\alias{coerce,SpatialPointsDataFrame,SpatialPixelsDataFrame-method} % here???
%\alias{coerce,SpatialPoints,data.frame-method}
%\alias{coerce,data.frame,SpatialPoints-method}
%\alias{coerce,matrix,SpatialPoints-method}
% \alias{coordinates,SpatialPoints-method}
\alias{show,SpatialPoints-method}
%\alias{SpatialPoints}
\alias{as.data.frame.SpatialPoints}
\alias{plot,SpatialPoints,missing-method}
\alias{summary,SpatialPoints-method}
\alias{print.SpatialPoints} 
\alias{summary.SpatialPoints}
\alias{rbind.SpatialPoints}
\alias{$,SpatialPoints-method}
\alias{$<-,SpatialPoints,character-method}
\alias{row.names.SpatialPoints}
\alias{coerce,ppp,SpatialPoints-method}


\title{Class "SpatialPoints" }
\description{  Class for (irregularly spaced) points }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SpatialPoints(x)}.
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"}, containing the
	coordinates (each row is a point) }
    \item{\code{bbox}:}{Object of class \code{"matrix"}, with bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}, projection string }
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialPoints")}: subsets the points; only
	rows (points) can be subsetted }
    \item{coerce}{\code{signature(from = "SpatialPoints", to = "data.frame")}: 
	retrieves the data part}
    \item{coerce}{\code{signature(from = "SpatialPoints", to = "SpatialPixels")}: equivalent to assigning gridded TRUE for a copy of the object}
    \item{coerce}{\code{signature(from = "SpatialPointsDataFrame", to = "SpatialPixelsDataFrame")}: equivalent to assigning gridded TRUE for a copy of the object}
    \item{coerce}{\code{signature(from = "data.frame", to = "SpatialPoints")}: sets
	coordinates, which may be in a data frame }
    \item{coerce}{\code{signature(from = "matrix", to = "SpatialPoints")}: set
	coordinates, which may be in a matrix }
    \item{coordinates}{\code{signature(obj = "SpatialPoints")}: retrieves
	the coordinates, as matrix }
    \item{plot}{\code{signature(x = "SpatialPoints", y = "missing")}: plot points}
    \item{summary}{\code{signature(object = "SpatialPoints")}: summarize object}
    \item{points}{\code{signature(x = "SpatialPoints")}: add point symbols to plot}
    \item{show}{\code{signature(object = "SpatialPoints")}: prints coordinates }
    \item{rbind}{\code{signature(object = "SpatialPoints")}: rbind-like method }  }
}

\section{plot method arguments}{
The plot method for \dQuote{SpatialPoints} objects takes the following arguments:
\describe{
\item{x}{object of class SpatialPoints}
\item{pch}{default 3; either an integer specifying a symbol or a single character to be used as the default in plotting points}
\item{axes}{default FALSE; a logical value indicating whether both axes should be drawn }
\item{add}{default FALSE; add to existing plot}
\item{xlim}{default NULL; the x limits (x1, x2) of the plot}
\item{ylim}{default NULL; the y limits of the plot}
\item{...}{passed through}
\item{setParUsrBB}{default FALSE; set the \code{par} \dQuote{usr} bounding box, see note in \link{Spatial-class}}
\item{cex}{default 1; numerical value giving the amount by which plotting text and symbols should be magnified relative to the default}
\item{col}{default 1; default plotting color}
\item{lwd}{default 1; line width}
\item{bg}{default 1; colour to be used for the background of the device region}
}
}


\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{SpatialPointsDataFrame-class}}
}
\examples{
x = c(1,2,3,4,5)
y = c(3,2,5,1,4)
S <- SpatialPoints(cbind(x,y))
S <- SpatialPoints(list(x,y))
S <- SpatialPoints(data.frame(x,y))
S
plot(S)
}
\keyword{classes}
