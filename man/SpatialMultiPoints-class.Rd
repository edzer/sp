\name{SpatialMultiPoints-class}
\docType{class}
\alias{SpatialMultiPoints-class}
\alias{[,SpatialMultiPoints-method}
\alias{coerce,SpatialMultiPoints,matrix-method}
\alias{coerce,SpatialMultiPoints,data.frame-method}
\alias{coerce,SpatialMultiPoints,SpatialPoints-method}
%\alias{coerce,SpatialPoints,data.frame-method}
%\alias{coerce,data.frame,SpatialPoints-method}
%\alias{coerce,matrix,SpatialPoints-method}
% \alias{coordinates,SpatialPoints-method}
\alias{show,SpatialMultiPoints-method}
%\alias{SpatialPoints}
\alias{as.data.frame.SpatialMultiPoints}
\alias{plot,SpatialMultiPoints,missing-method}
\alias{summary,SpatialMultiPoints-method}
\alias{print.SpatialMultiPoints} 
\alias{summary.SpatialMultiPoints}
\alias{rbind.SpatialMultiPoints}
\alias{$,SpatialMultiPoints-method}
\alias{$<-,SpatialMultiPoints,character-method}
\alias{row.names.SpatialMultiPoints}

\title{Class "SpatialMultiPoints" }
\description{  Class for (irregularly spaced) MultiPoints }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SpatialPoints(x)}.
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"list"}, containing the
	coordinates of point sets (each list element is a matrix) }
    \item{\code{bbox}:}{Object of class \code{"matrix"}, with bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}, projection string }
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialMultiPoints")}: subsets point sets} 
    \item{coerce}{\code{signature(from = "SpatialPoints", to = "data.frame")}: 
	coerce to data.frame}
    \item{coordinates}{\code{signature(obj = "SpatialMultiPoints")}: retrieves
	all the coordinates, as one single matrix }
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
  \code{\link{SpatialMultiPointsDataFrame-class}}
  \code{\link{SpatialPoints-class}}
}
\examples{
cl1 = cbind(rnorm(3, 10), rnorm(3, 10))
cl2 = cbind(rnorm(5, 10), rnorm(5,  0))
cl3 = cbind(rnorm(7,  0), rnorm(7, 10))

mp = SpatialMultiPoints(list(cl1, cl2, cl3))
plot(mp, col = 2, cex = 1, pch = 1:3)
mp
mp[1:2]

print(mp, asWKT=TRUE, digits=3)
}
\keyword{classes}
