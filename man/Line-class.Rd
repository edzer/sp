\name{Line-class}
\docType{class}
\alias{Line-class}
% \alias{coordinates,Line-method}

\title{Class "Line" }
\description{ class for line objects }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Line", ...)},
or (preferred) by calls to the function \link{Line}
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"}, containing the
	line coordinates }
  }
}
\section{Methods}{
  \describe{
    \item{coordinates}{\code{signature(obj = "Line")}: retrieve coordinates
	from line }
    \item{lines}{\code{signature(x = "Line")}: add lines to a plot }
  }
}
\author{ Roger Bivand, Edzer Pebesma }

\seealso{
  \link{Lines-class}, \link{SpatialLines-class}
}
\keyword{classes}
