\name{SpatialPixelsDataFrame-class}
\docType{class}
\alias{SpatialPixelsDataFrame-class}
\alias{[,SpatialPixelsDataFrame-method}
\alias{show,SpatialPixelsDataFrame-method}
% \alias{SpatialPixelsDataFrame}
\alias{plot.SpatialPixelsDataFrame}
\alias{print.summary.SpatialPixelsDataFrame}
\alias{coerce,SpatialPixelsDataFrame,SpatialPointsDataFrame-method}
\alias{coerce,SpatialPixelsDataFrame,SpatialGridDataFrame-method}
\alias{coerce,SpatialPixelsDataFrame,SpatialPolygonsDataFrame-method}
\alias{coerce,SpatialPixelsDataFrame,data.frame-method}
\alias{coerce,SpatialPixelsDataFrame,matrix-method}
\alias{as.data.frame.SpatialPixelsDataFrame}
\alias{rbind.SpatialPixelsDataFrame}
\alias{dim.SpatialPixelsDataFrame}

\title{Class "SpatialPixelsDataFrame" }
\description{  Class for spatial attributes that have spatial locations
on a regular grid. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{as(x,
"SpatialPixelsDataFrame")}, where \code{x} is of class
\link{SpatialPointsDataFrame-class}, or by importing through rgdal.  
Ordered full grids are stored instead or unordered non-NA cells; 
}

\section{Slots}{
  \describe{
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection }
    \item{\code{coords}:}{see \link{SpatialPoints}; points slot }
	\item{\code{coords.nrs}}{see \link{SpatialPointsDataFrame}}
    \item{\code{grid}:}{see \link{GridTopology-class}; grid parameters }
    \item{\code{grid.index}:}{integer; index of points in the list to points
	in the full (ordered) grid. x cycles fastest; all coordinates increase
	from low to hight except y, which decreases from high to low }
    \item{\code{data}:}{Object of class data.frame, containing the attribute data }
  }
}
\section{Extends}{
Class \code{"SpatialPixels"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialPixels"}.
}
\section{Methods}{
  \describe{
    \item{coordinates}{\code{signature(x = "SpatialPixelsDataFrame")}: retrieves coordinates }
    \item{[}{\code{signature(x = "SpatialPixelsDataFrame")}: selects row(s) and/or attribute(s), and returns an
	object of class \code{SpatialPixelsDataFrame}; rows refer here to the pixel
	numbers, not grid lines. For selecting a square block in a grid, coerce to
	a \link{SpatialGridDataFrame-class} first, and use \code{[} on that object}
	\item{as.matrix}{\code{signature(x = "SpatialPixelsDataFrame")}: coerce to matrix }
	\item{rbind}{\code{signature(x = "SpatialPixelsDataFrame")}: rbind-like method }
  }
}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{SpatialPixels-class}}, which does not contain the attribute data
}
\examples{
data(meuse.grid) # only the non-missing valued cells
coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(meuse.grid) <- TRUE # promote to SpatialPixelsDataFrame
meuse.grid[["idist"]] = 1 - meuse.grid[["dist"]] # assigns new attribute
image(meuse.grid["idist"]) # note the single [

# toy example:
df = data.frame(z = c(1:6,NA,8,9),
    xc = c(1,1,1,2,2,2,3,3,3),
    yc = c(rep(c(0, 1.5, 3),3)))
coordinates(df) = ~xc+yc
gridded(df) = TRUE
image(df["z"])
# draw labels to verify:
cc = coordinates(df)
z=df[["z"]]
zc=as.character(z)
zc[is.na(zc)]="NA"
text(cc[,1],cc[,2],zc)

}
\keyword{classes}
