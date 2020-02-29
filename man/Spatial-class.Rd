\name{Spatial-class}
\docType{class}
\alias{Spatial-class}
\alias{Spatial}
\alias{print.summary.Spatial}
\alias{subset.Spatial}
\alias{cbind.Spatial}
\alias{head.Spatial}
\alias{tail.Spatial}
\alias{summary,Spatial-method}
\alias{plot,Spatial,missing-method}
\alias{getParUsrBB}
\alias{setParUsrBB}
\alias{coordinates<-,Spatial-method}
\alias{[[<-,Spatial,ANY,missing-method}
\alias{[<-,Spatial-method}
\alias{[[,Spatial,ANY,missing-method}
\alias{$<-,Spatial-method}
\alias{$,Spatial-method}
\alias{im-class}
\alias{owin-class}
\alias{ppp-class}
\alias{psp-class}

\title{Class "Spatial"}
\description{ An abstract class from which useful spatial classes are
derived }
\section{Objects from the Class}{
are never to be generated; only derived classes can be meaningful }
\section{Slots}{
  \describe{
    \item{\code{bbox}:}{Object of class \code{"matrix"}; 2-column matrix
	holding the minimum in first and maximum in second column for the
	x-coordinate (first row), y-coordinate (second row) and optionally,
	for points and grids only, further coordinates. The constructed 
	Spatial object will be invalid if any bbox values are NA or infinite. The column names must be \code{c("min", "max")}}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}. The name of this slot was chosen to reflect the use of Proj.4 strings to represent coordinate reference systems (CRS). The slot name will continue to be used, but as PROJ >= 6 and GDAL >= 3 are taken into use for reading files and for projection and transformation, the Proj.4 string CRS representation is being supplemented by a WKT2 (2019) representation. The reason for the modification is that important keys in the Proj.4 string representation are being deprecated in PROJ >= 6 and GDAL >= 3. Legacy \code{"CRS"}  objects hold only a valid Proj.4 string, which can be used for unprojecting or reprojecting coordinates; it is initialised to NA. If the \code{"CRS"} object is instantiated using \code{CRS()} with \pkg{rgdal} using PROJ >= 6 and GDAL >= 3, the object may also have a WKT2 (2019) string carried as a \code{comment}. Non-NA strings may be checked for validity in the rgdal package, but attempts to assign a string containing "longlat" to data extending beyond longitude [-180, 360] or lattitude [-90, 90] will be stopped or warned, use \code{\link{set_ll_warn}} to warn rather than stop, and \code{\link{set_ll_TOL}} to change the default tolerance for the range exceedance tests.}
  }
}
\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "Spatial")}: retrieves the bbox element }
    \item{dimensions}{\code{signature(obj = "Spatial")}: retrieves the number
	of spatial dimensions spanned }
    \item{gridded}{\code{signature(obj = "Spatial")}: logical, tells whether
	the data is on a regular spatial grid }
    \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	for spatial objects; does nothing but setting up a plotting region choosing
	a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	\item{summary}{\code{signature(object = "Spatial")}: summarize object}
	\item{\code{$}}{ retrieves attribute column }
	\item{\code{$<-}}{ sets or replaces attribute column, or promote a
	geometry-only object to an object having an attribute }
  }
}

\usage{
Spatial(bbox, proj4string = CRS(as.character(NA)))
\method{subset}{Spatial}(x, subset, select, drop = FALSE, \dots)
}

\arguments{
\item{bbox}{a bounding box matrix}
\item{proj4string}{a CRS object}
\item{x}{object of class Spatial}
\item{subset}{see \link{subset.data.frame}}
\item{select}{see \link{subset.data.frame}}
\item{drop}{see \link{subset.data.frame}}
\item{...}{passed through}
}

\section{plot method arguments}{
The plot method for \dQuote{Spatial} objects takes the following arguments:
\describe{
\item{x}{object of class Spatial}
\item{xlim}{default NULL; the x limits (x1, x2) of the plot}
\item{ylim}{default NULL; the y limits of the plot}
\item{asp}{default NA; the y/x aspect ratio}
\item{axes}{default FALSE; a logical value indicating whether both axes should be drawn }
\item{bg}{default \code{par("bg")}; colour to be used for the background of the device region}
\item{xaxs}{ The style of axis interval calculation to be used for the x-axis}
\item{yaxs}{ The style of axis interval calculation to be used for the y-axis}
\item{lab}{A numerical vector of the form \code{c(x, y, len)} which modifies the default way that axes are annotated}
\item{setParUsrBB}{default FALSE; set the \code{par} \dQuote{usr} bounding box; see below}
\item{bgMap}{object of class \code{ggmap}, or returned by function \code{RgoogleMaps::GetMap}}
\item{expandBB}{ numeric; factor to expand the plotting region default: \code{bbox(x)} with on
each side (1=below, 2=left, 3=above and 4=right); defaults to \code{c(0,0,0,0)}; 
setting \code{xlim} or \code{ylim} overrides this. }
\item{...}{passed through}
}
}

\author{ r-spatial team; Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}
Roger Bivand, Barry Rowlingson, Virgilio G\'omez-Rubio
}
\note{  
	The default aspect for map plots is 1; if however data are not
	projected (coordinates are longlat), the aspect is by default set to
	1/cos(My * pi)/180) with My the y coordinate of the middle of the map
	(the mean of ylim, which defaults to the y range of bounding box).

The argument \code{setParUsrBB} may be used to pass the logical value \code{TRUE} to functions within \code{plot.Spatial}. When set to \code{TRUE}, par(\dQuote{usr}) will be overwritten with \code{c(xlim, ylim)}, which defaults to the bounding box of the spatial object. This is only needed in the particular context of graphic output to a specified device with given width and height, to be matched to the spatial object, when using par(\dQuote{xaxs}) and par(\dQuote{yaxs}) in addition to \code{par(mar=c(0,0,0,0))}.
}

\section{Warning }{this class is not useful in itself, but all spatial
classes in this package derive from it }

\seealso{
  \code{\link{SpatialPoints-class}},
  \code{\link{SpatialGrid-class}},
%  \code{\link{SpatialPolygons-class}},
%  \code{\link{SpatialArcs-class}},
  \code{\link{SpatialPointsDataFrame-class}},
  \code{\link{SpatialGridDataFrame-class}}
%  \code{\link{SpatialPolygonsDataFrame-class}},
}
\examples{
o <- new("Spatial")
proj4string(o) <- CRS("+init=epsg:27700")
if (!is.null(comment(slot(o, "proj4string")))) {
  cat(strsplit(wkt(o), "\\n")[[1]], sep="\n")
  cat(strsplit(wkt(slot(o, "proj4string")), "\\n")[[1]], sep="\n")
}
}

\keyword{classes}
