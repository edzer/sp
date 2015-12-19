\name{DMS-class}
\docType{class}
\alias{DMS-class}
\alias{print.DMS}
\alias{show,DMS-method}
\alias{coerce,DMS-method}
\alias{as.double.DMS}
\alias{as.numeric.DMS}
\title{Class "DMS" for degree, minute, decimal second values}
\description{The class provides a container for coordinates stored as degree, minute, decimal second values.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DMS", ...)}, converted from decimal degrees using \code{dd2dms()}, or converted from character strings using \code{char2dms()}. 
}
\section{Slots}{
  \describe{
    \item{\code{WS}:}{Object of class \code{"logical"} TRUE if input value negative}
    \item{\code{deg}:}{Object of class \code{"numeric"} degrees}
    \item{\code{min}:}{Object of class \code{"numeric"} minutes}
    \item{\code{sec}:}{Object of class \code{"numeric"} decimal seconds}
    \item{\code{NS}:}{Object of class \code{"logical"} TRUE if input value is a Northing}
  }
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "DMS", to = "numeric")}: convert to decimal degrees }
    \item{show}{\code{signature(object = "DMS")}: print data values }
  }
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no} }

\seealso{\code{\link{char2dms}}, \code{\link{dd2dms}}}


\examples{
data(state)
dd2dms(state.center$x)
dd2dms(state.center$y, NS=TRUE)
as.numeric(dd2dms(state.center$y))
as(dd2dms(state.center$y, NS=TRUE), "numeric")
as.numeric.DMS(dd2dms(state.center$y))
state.center$y
}
\keyword{classes}
\keyword{spatial}
