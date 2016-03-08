\name{SpatialGridDataFrame-class}
\docType{class}
\alias{SpatialGridDataFrame-class}
\alias{[,SpatialGridDataFrame-method}
\alias{show,SpatialGridDataFrame-method}
\alias{cbind.SpatialGridDataFrame}
% \alias{SpatialGridDataFrame}
\alias{plot.SpatialGridDataFrame}
\alias{print.summary.SpatialGridDataFrame}
\alias{as.array.SpatialGridDataFrame}
\alias{coerce,SpatialGridDataFrame,SpatialPointsDataFrame-method}
\alias{coerce,SpatialGridDataFrame,SpatialPixelsDataFrame-method}
\alias{coerce,SpatialGridDataFrame,SpatialPolygonsDataFrame-method}
\alias{coerce,SpatialGridDataFrame,data.frame-method}
\alias{coerce,SpatialGridDataFrame,matrix-method}
\alias{coerce,SpatialGridDataFrame,array-method}
\alias{as.data.frame.SpatialGridDataFrame}
\alias{dim.SpatialGridDataFrame}
\alias{coerce,im,SpatialGridDataFrame-method}
\alias{coerce,ppp,SpatialGridDataFrame-method}
\alias{plot,SpatialGridDataFrame,missing-method}
\alias{plot,SpatialPixelsDataFrame,missing-method}

\title{Class "SpatialGridDataFrame" }
\description{  Class for spatial attributes that have spatial locations
on a (full) regular grid. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{as(x,
"SpatialGridDataFrame")}, where \code{x} is of class
\link{SpatialPixelsDataFrame-class}, or by importing through rgdal.  
Ordered full grids are stored instead or unordered non-NA cells; 
}

\section{Slots}{
  \describe{
    \item{\code{grid}:}{see \link{GridTopology-class}; grid parameters }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection }
    \item{\code{data}:}{Object of class data.frame, containing attribute data }
  }
}
\section{Extends}{
Class \code{"SpatialGrid"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialGrid"}.
}
\section{Methods}{
  \describe{
    \item{coordinates}{\code{signature(x = "SpatialGridDataFrame")}: retrieves (and calculates!) coordinates }
    \item{[}{\code{signature(x = "SpatialGridDataFrame")}: selects rows, columns, and attributes; returns an
	object of class \code{SpatialGridDataFrame}}
	\item{as.matrix}{\code{signature(x = "SpatialGridDataFrame")}: coerce to matrix; increasing col index corresponds to decreasing y coordinate, row index increases with coordinate index }
	\item{as.array}{\code{signature(x = "SpatialGridDataFrame")}: coerce to array; increasing array index for the second dimension corresponds to decreasing coordinates, all other coordinate dimensions increase with array index }
	\item{cbind}{\code{signature(...)}: if arguments have identical topology, combine their
	attribute values}
  }
}

\section{Plot method arguments}{
The plot method for \dQuote{SpatialPixelsDataFrame} or \dQuote{SpatialGridDataFrame} 
objects takes the following arguments:
\describe{
\item{x}{object of class \link{SpatialPixelsDataFrame} or \link{SpatialGridDataFrame}}
\item{...}{ arguments passed on to \link{image.SpatialGridDataFrame}}
\item{attr}{ integer or character, indicating the attribute variable to be plotted; default 1}
\item{col}{ color ramp to be used; default \code{bpy.colors(100)} for continuous, or
   \code{RColorBrewer::brewer.pal(nlevels(x[[1]]), "Set2")} for factor variables}
\item{breaks}{ for continous attributes: values at which color breaks should take place }
\item{zlim}{ for continuous attributes: 
  numeric of length 2, specifying the range of attribute values to be plotted; 
  default to data range \code{range(as.numeric(x[[attr]])[is.finite(x[[attr]])])}}
\item{axes}{ logical: draw x and y axes? default \code{FALSE}}
\item{xaxs}{character, default "i", see \link{par}}
\item{yaxs}{character, default equal to \code{xaxs}, see \link{par}}
\item{at}{numeric or NULL, values at which axis tics and labes should be drawn; default NULL (use \link{pretty})}
\item{border}{color, to be used for drawing grid lines; default NA (don't draw grid lines)}
\item{axis.pos}{integer, 1-4; default 4, see \link{axis}}
\item{add.axis}{logical: draw axis along scale? default \code{TRUE}}
\item{what}{what to draw: \code{"image"}, \code{"scale"}, or \code{"both"}; default "both"}
\item{scale.size}{size for the scale bar; use \link{lcm} to specify in absolute size, or a numeric
value such as \code{1/6} to specify relative size; default \code{lcm(2.8)}}
\item{scale.shrink}{ non-negative numeric indicating the amount to shrink the scale length, default 0}
\item{scale.frac}{for categorical attributes: numeric between 0 and 1, indicating the scale width, default 0.3}
\item{scale.n}{for categorical attributes: integer, indicating how many scale categories should fill a complete width; default 15}
}
}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{SpatialGrid-class}}, which does not contain the attribute data,
  and \code{\link{SpatialPixelsDataFrame-class} which holds possibly incomplete
  grids }
}
\examples{
data(meuse.grid) # only the non-missing valued cells
coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(meuse.grid) <- TRUE # promote to SpatialPixelsDataFrame
x = as(meuse.grid, "SpatialGridDataFrame") # creates the full grid
x[["idist"]] = 1 - x[["dist"]] # assigns new attribute
image(x["idist"]) # note the single [ for attribute selection

# toy example:
df = data.frame(z = c(1:6,NA,8,9),
    xc = c(1,1,1,2,2,2,3,3,3),
    yc = c(rep(c(0, 1.5, 3),3)))
coordinates(df) = ~xc+yc
gridded(df) = TRUE
df = as(df, "SpatialGridDataFrame") # to full grid
image(df["z"])
# draw labels to verify:
cc = coordinates(df)
z=df[["z"]]
zc=as.character(z)
zc[is.na(zc)]="NA"
text(cc[,1],cc[,2],zc)

# the following is weird, but illustrates the concept of row/col selection:
fullgrid(meuse.grid) = TRUE
image(meuse.grid)
image(meuse.grid[20:70, 10:70, "dist"], add = TRUE, col = bpy.colors())

# as.matrix, as.array
sgdim = c(3,4)
SG = SpatialGrid(GridTopology(rep(0,2), rep(10,2), sgdim))
SGDF = SpatialGridDataFrame(SG, data.frame(val = 1:12))
as.array(SGDF)
as.matrix(SGDF)
as(SGDF, "array")
}
\keyword{classes}
