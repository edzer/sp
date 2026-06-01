\name{Line-class}
\docType{class}
\alias{Line-class}
% \alias{coordinates,Line-method} % -> in methods?coordinates

\title{Class \code{"Line"}}
\description{ class for line objects }
\section{Objects from the Class}{
Objects can be created by calls to the function \code{\link{Line}}.
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"}, containing the
	line coordinates }
  }
}
\section{Methods}{
  There are methods for
  \code{\link[=bbox-methods]{bbox}},
  \code{\link[=coordinates-methods]{coordinates}},
  \code{\link[=coordnames-methods]{coordnames}}, and
  \code{\link[=spsample-methods]{spsample}}.

  There is also a \code{\link{lines}} method to add lines to a plot.
}
\author{ Roger Bivand, Edzer Pebesma }

\seealso{
  The generator function \code{\link{Line}}, and the related classes
  \code{"\linkS4class{Lines}"} and \code{"\linkS4class{SpatialLines}"}.
}
\keyword{classes}
