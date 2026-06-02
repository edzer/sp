\name{Lines-class}
\docType{class}
\alias{Lines-class}
% \alias{coordinates,Lines-method} % -> in methods?coordinates

\title{Class \code{"Lines"}}
\description{ class for sets of line objects }
\section{Objects from the Class}{
Objects can be created by calls to the function \code{\link{Lines}}.
}
\section{Slots}{
  \describe{
    \item{\code{Lines}:}{Object of class \code{"list"}, containing elements
	of class \code{"\linkS4class{Line}"}}
    \item{\code{ID}:}{\code{"character"} vector of length one, with unique identifier string}
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
  The generator function \code{\link{Lines}}, and the related classes
  \code{"\linkS4class{Line}"} and \code{"\linkS4class{SpatialLines}"}.
}
\keyword{classes}
