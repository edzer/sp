\name{char2dms}
\alias{char2dms}
\alias{dd2dms}
\alias{coerce,DMS,numeric-method}
\alias{coerce,DMS,character-method}
\alias{as.character.DMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert character vector to DMS-class object}
\description{
  These two helper functions convert character vectors and decimal degree vectors to the DMS-class representation of degrees, minutes, and decimal seconds. "DMS" objects cannot contain NAs.
}

\section{Methods}{\describe{

\item{from = "DMS", to = "numeric"}{coerce a \code{"DMS"} object to a \code{"numeric"} vector}

\item{from = "DMS", to = "character"}{coerce a \code{"DMS"} object to a \code{"character"} vector (the \code{as.character.DMS} S3 method is also available)}
}}
\usage{
char2dms(from, chd = "d", chm = "'", chs = "\"")
dd2dms(dd, NS = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{character vector of degree, minute, decimal second data}
  \item{chd}{degree character terminator}
  \item{chm}{minute character terminator}
  \item{chs}{second character terminator}
  \item{dd}{numeric vector of decimal degrees}
  \item{NS}{logical, TRUE for north/south decimal degrees, FALSE for east/west decimal degrees}
}
\details{
  In char2dms, the input data vector should use a regular format, such as that used in the PROJ.4 library, with a trailing capital (NSWE) indicating compass direction.
}
\value{
  Both functions return a "DMS" object.
}

\author{Roger Bivand \email{Roger.Bivand@nhh.no} }
 
\seealso{\code{\link{DMS-class}}}
\examples{
data(state)
str(state.center$y)
stateN <- dd2dms(state.center$y, NS=TRUE)
str(attributes(stateN))
ch.stateN <- as.character(stateN)
str(ch.stateN)
stateNa <- char2dms(ch.stateN)
str(attributes(stateNa))
ch.stateN <- as(stateN, "character")
str(ch.stateN)
stateNa <- char2dms(ch.stateN)
str(attributes(stateNa))
}
\keyword{spatial}
