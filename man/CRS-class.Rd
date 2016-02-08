\name{CRS-class}
\docType{class}
\alias{CRS-class}
\alias{CRS}
\alias{CRSargs}
\alias{identicalCRS}
\alias{print.CRS}
\alias{show,CRS-method}
\title{Class "CRS" of coordinate reference system arguments}
\description{Interface class to the PROJ.4 projection system. The class is defined as an empty stub accepting value NA in the sp package. If the rgdal package is available, then the class will permit spatial data to be associated with coordinate reference systems. The arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. Note that only \dQuote{+proj=longlat +ellps=WGS84} is accepted for geographical coordinates, which must be ordered (eastings, northings); the \dQuote{+ellps=} definition must be given (or expanded internally from a given \dQuote{+datum=} value) for recent versions of the PROJ.4 library, and should be set to an appropriate value.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{CRS("projargs")}, where "projargs" is a valid string of PROJ.4 arguments. The initiation function calls the PROJ.4 library to verify the argument set against those known in the library, returning error messages where necessary. The function \code{CRSargs()} can be used to show the expanded argument list used by the PROJ.4 library.
}
\section{Slots}{
  \describe{
    \item{\code{projargs}:}{Object of class \code{"character"}: projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "CRS")}: print projection arguments in object }
  }
}
\usage{
CRS(projargs, doCheckCRSArgs=TRUE)
identicalCRS(x,y)
}
\arguments{
\item{projargs}{A character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation; if the projection is unknown, use \code{as.character(NA)}, it may be missing or an empty string of zero length and will then set to the missing value.}
\item{doCheckCRSArgs}{default TRUE, must be set to FALSE by package developers including \code{CRS} in an S4 class definition to avoid uncontrolable loading of the \pkg{rgdal} namespace}
\item{x}{object having a \link{proj4string} method,
or if \code{y} is missing, list with objects that have a \code{proj4string} method}
\item{y}{object of class \link{Spatial}, or having a \link{proj4string} method}
}
\value{
\code{CRS} returns on success an object of class
\link{CRS}. \code{identicalCRS} returns a logical, indicating
whether \code{x} and \code{y} have identical CRS, or if \code{y}
is missing whether all objects in list \code{x}
have identical CRS.
}
\references{\url{https://github.com/OSGeo/proj.4}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{
Lists of projections may be seen by using the programs installed with the PROJ.4 library, in particular proj and cs2cs; with the latter, -lp lists projections, -le ellipsoids, -lu units, and -ld datum(s) known to the installed software (available in \pkg{rgdal} using \code{projInfo}). These are added to in successive releases, so tracking the website or compiling and installing the most recent revisions will give the greatest choice. Finding the very important datum transformation parameters to be given with the +towgs84 tag is a further challenge, and is essential when the datums used in data to be used together differ. Tracing projection arguments is easier now than before the mass ownership of GPS receivers raised the issue of matching coordinates from different argument sets (GPS output and paper map, for example). See \code{\link[rgdal]{GridsDatums}}, \code{\link[rgdal]{make_EPSG}} and \code{\link[rgdal]{showEPSG}} for help in finding CRS definitions.

The 4.9.1 release of PROJ.4 omitted a small file of defaults, leading to reports of \dQuote{major axis or radius = 0 or not given} errors. From 0.9-3, rgdal checks for the presence of this file (proj_def.dat), and if not found, and under similar conditions to those used by PROJ.4, adds \dQuote{+ellps=WGS84} to the input string being checked by \code{checkCRSArgs} The \dQuote{+no_defs} tag ignores the file of defaults, and the default work-around implemented to get around this problem; strings including \dQuote{init} and \dQuote{datum} tags also trigger the avoidance of the work-around. Now messages are issued when a candidate CRS is checked; they may be suppressed using \code{suppressMessages}.}


\examples{
CRS()
CRS("")
CRS(as.character(NA))
CRS("+proj=longlat +datum=WGS84")
if (require(rgdal)) {
  print(CRSargs(CRS("+proj=longlat +datum=NAD27")))
  print(CRSargs(CRS("+init=epsg:4267")))
  print(CRSargs(CRS("+init=epsg:26978")))
  print(CRSargs(CRS(paste("+proj=sterea +lat_0=52.15616055555555",
 "+lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel",
 " +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m"))))
  print(CRSargs(CRS("+init=epsg:28992")))
}
# see http://trac.osgeo.org/gdal/ticket/1987
}
\keyword{classes}
\keyword{spatial}
