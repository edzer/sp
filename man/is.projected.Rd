\name{is.projected}
\alias{is.projected}
\alias{proj4string}
\alias{proj4string<-}
\alias{is.projected,Spatial-method}
\alias{proj4string,Spatial-method}
\alias{wkt}
\alias{wkt,Spatial-method}
\alias{proj4string<-,Spatial,CRS-method}
\alias{proj4string<-,Spatial,character-method}
\alias{get_ll_warn}
\alias{get_ll_TOL}
\alias{set_ll_warn}
\alias{set_ll_TOL}
\alias{get_ReplCRS_warn}
\alias{set_ReplCRS_warn}

\title{ Sets or retrieves projection attributes on classes extending
SpatialData }
\description{ Sets or retrieves projection attributes on classes extending
SpatialData; set or retrieve option value for error or warning on exceedance of geographical coordinate range, set or retrieve option value for exceedance tolerance of geographical coordinate range. Note that only \dQuote{+proj=longlat +ellps=WGS84} is accepted for geographical coordinates, which must be ordered (eastings, northings); the \dQuote{+ellps=} definition must be given (or expanded internally from a given \dQuote{+datum=} value) for recent versions of the PROJ library, and should be set to an appropriate value.

From release 6 of the PROJ library, when used in building \pkg{rgdal} with GDAL >= 3, the \code{+datum=} key in the Proj.4 string CRS representation is deprecated, and the \code{+towgs84=} and \code{+nadgrids=} keys may be deprecated soon. For this reason, \pkg{sp}, \pkg{rgdal} and \pkg{sf} are starting to use WKT2 (2019) string representations. In \pkg{sp}, the \code{"CRS"} object in itself remains unchanged, but the content of its \code{"projargs"} slot may be degraded. To work around the degradation, a \code{comment} is added around the \code{"CRS"} object containing a WKT2 (2019) string when \pkg{rgdal} is available and built with PROJ >= 6 and GDAL >=3. The \code{wkt()} accessor function returns the WKT2 (2019) string comment belonging to the \code{"CRS"} object.

}
\usage{
is.projected(obj)
proj4string(obj)
proj4string(obj) <- value
wkt(obj)
get_ll_warn()
get_ll_TOL()
get_ReplCRS_warn()
set_ll_warn(value)
set_ll_TOL(value)
set_ReplCRS_warn(value)
}
\arguments{
  \item{obj}{An object of class or extending \link{Spatial-class}}
  \item{value}{For \code{proj4string} CRS object, containing a valid proj4 string; attempts to assign an object containing \dQuote{longlat} to data extending beyond longitude [-180, 360] or lattitude [-90, 90] will be stopped. For \code{set_ll_warn} a single logical value, if FALSE (default) error on range exceedance, if TRUE, warning. For \code{set_ll_TOL} the value of the power of \code{.Machine$double.eps} (default 0.25) to use as tolerance in testing range exceedance. \code{set_ReplCRS_warn} may be used to turn off warnings issued when changing object CRS with the \code{proj4string} replacement method (by setting \code{value=FALSE}).}
}

\value{ \code{is.projected} returns a logical that may be \code{NA}; 
\code{proj4string} returns a character vector of length 1.} 

\details{ proj4 strings are operative through CRAN package rgdal. For strings defined as \dQuote{longlat}, the minimum longitude should be -180, the maximum longitude 360, the minimum latitude -90, and the maximum latitude 90. Note that the \code{proj4string} replacement method does not project spatial data - for this use \code{spTransform} methods in the rgdal package.}

\author{
  Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}
}

\seealso{ \link{CRS} }

\examples{
o <- new("Spatial")
proj4string(o) <- CRS("+init=epsg:27700")
if (!is.null(comment(slot(o, "proj4string")))) {
  cat(strsplit(wkt(o), "\\n")[[1]], sep="\n")
  cat(strsplit(wkt(slot(o, "proj4string")), "\\n")[[1]], sep="\n")
}
}

\keyword{spatial}
