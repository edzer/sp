\name{SpatialLines-class}
\docType{class}
\alias{SpatialLines-class}
% \alias{coordinates,SpatialLines-method}
\alias{[,SpatialLines-method}
\alias{plot,SpatialLines,missing-method}
\alias{summary,SpatialLines-method}
\alias{rbind.SpatialLines}
\alias{coerce,SpatialLines,SpatialPoints-method}
\alias{coerce,SpatialLines,SpatialPointsDataFrame-method}
\alias{coerce,Lines,SpatialPoints-method}
\alias{coerce,Lines,SpatialMultiPoints-method}
\alias{coerce,SpatialLines,SpatialMultiPoints-method}
\alias{coerce,deldir,SpatialLines-method}
\alias{row.names.SpatialLines}

\title{a class for spatial lines }
\description{ a class that holds spatial lines }
\section{Objects from the Class}{
hold a list of Lines objects; each Lines object holds a list
of Line (line) objects.
}
\section{Slots}{
  \describe{
    \item{\code{lines}:}{Object of class \code{"list"}; list members are all of
	class \link{Lines-class}}
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(obj = "SpatialLines")}: select subset of (sets of) lines; NAs are not permitted in the row index}
	\item{coordinates}{ value is a list of lists with matrices }
	\item{plot}{\code{signature(x = "SpatialLines", y = "missing")}: 
	plot lines in SpatialLines object}
	\item{lines}{\code{signature(x = "SpatialLines")}: 
	add lines in SpatialLines object to a plot }
	\item{rbind}{\code{signature(object = "SpatialLines")}: 
	  rbind-like method, see notes }
	\item{summary}{\code{signature(object = "SpatialLines")}: 
	summarize object }
  }
}
\note{
\code{rbind} calls the function \code{\link{SpatialLines}}, where it is 
checked that all IDs are unique. If \code{rbind}-ing \code{SpatialLines} without 
unique IDs, it is possible to set the argument \code{makeUniqueIDs = TRUE}, although
it is preferred to change these explicitly with \code{\link{spChFIDs}}.
}

\section{plot method arguments}{
The plot method for \dQuote{SpatialLines} objects takes the following arguments:
\describe{
\item{x}{object of class SpatialLines}
\item{xlim}{default NULL; the x limits (x1, x2) of the plot}
\item{ylim}{default NULL; the y limits of the plot}
\item{col}{default 1; default plotting color}
\item{lwd}{default 1; line width}
\item{lty}{default 1; line type}
\item{add}{default FALSE; add to existing plot}
\item{axes}{default FALSE; a logical value indicating whether both axes should be drawn }
\item{lend}{default 0; line end style}
\item{ljoin}{default 0; line join style}
\item{lmitre}{default 10; line mitre limit}
\item{...}{passed through}
\item{setParUsrBB}{set the \code{par} \dQuote{usr} bounding box, see note in \link{Spatial-class}}
}
}

\author{ Roger Bivand, Edzer Pebesma }

\seealso{
	\link{Line-class}, \link{Lines-class}
}
\examples{ 
# from the sp vignette:
l1 = cbind(c(1,2,3),c(3,2,2))
rownames(l1) = letters[1:3]
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
rownames(l1a) = letters[1:3]
l2 = cbind(c(1,2,3),c(1,1.5,1))
rownames(l2) = letters[1:3]
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
Sl = SpatialLines(list(S1,S2))
summary(Sl)
plot(Sl, col = c("red", "blue"))
}
\keyword{classes}
