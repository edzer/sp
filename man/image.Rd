\name{image.SpatialGridDataFrame}
\alias{image.SpatialGridDataFrame}
\alias{image.SpatialPixelsDataFrame}
\alias{image.SpatialPixels}
\alias{contour.SpatialGridDataFrame}
\alias{contour.SpatialPixelsDataFrame}
\alias{as.image.SpatialGridDataFrame}
\alias{image2Grid}
\alias{imageScale}
\title{ image gridded spatial data, or convert to format for image }
\description{
Convert gridded data in SpatialGridDataFrame to image format; call
image on data in SpatialGridDataFrame format. The aspect ratio is set as either 1 for projected data, or stretched by distance from Equator for geographical coordinates.
}
\usage{
\method{image}{SpatialGridDataFrame}(x, attr = 1, xcol = 1, ycol = 2, 
	col = heat.colors(12), red=NULL, green=NULL, blue=NULL,
        axes = FALSE, xlim = NULL,
	ylim = NULL, add = FALSE, ..., asp = NA, setParUsrBB=FALSE,
        interpolate = FALSE, angle = 0, 
	useRasterImage = (!.isSDI() && missing(breaks)), breaks,
	zlim = range(as.numeric(x[[attr]])[is.finite(x[[attr]])]))
\method{image}{SpatialPixelsDataFrame}(x, ...) 
\method{image}{SpatialPixels}(x, ...) 
\method{contour}{SpatialGridDataFrame}(x, attr = 1, xcol = 1, ycol = 2,
	col = 1, add = FALSE, xlim = NULL, ylim = NULL, axes = FALSE,
         ..., setParUsrBB = FALSE)
\method{contour}{SpatialPixelsDataFrame}(x, ...) 
as.image.SpatialGridDataFrame(x, xcol = 1, ycol = 2, attr = 1)
image2Grid(im, p4 = as.character(NA), digits=10)
}
\arguments{
\item{x}{ object of class \link{SpatialGridDataFrame} }
\item{attr}{ column of attribute variable; this may be
the column name in the data.frame of \code{data} (as.data.frame(data)), or
a column number }
\item{xcol}{ column number of x-coordinate, in the coordinate matrix }
\item{ycol}{ column number of y-coordinate, in the coordinate matrix }
\item{col}{a vector of colors }
\item{red,green,blue}{columns names or numbers given instead of the \code{attr} argument when the data represent an image encoded in three colour bands on the 0-255 integer scale; all three columns must be given in this case, and the attribute values will be constructed using function \code{rgb} }
%\item{asp}{ aspect ratio of unit x and unit y axis }
\item{axes}{ logical; should coordinate axes be drawn? }
\item{xlim}{ x-axis limits }
\item{ylim}{ y-axis limits }
\item{zlim}{ data limits for plotting the (raster, attribute) values }
\item{add}{ logical; if FALSE, the image is added to the plot layout setup
by \code{plot(as(x, "Spatial"),axes=axes,xlim=xlim,ylim=ylim,asp=asp)}
which sets up axes and plotting region; if TRUE, the image is added
to the existing plot. }
\item{...}{ arguments passed to \link{image}, see examples }
\item{asp}{ aspect ratio to be used for plot }
\item{setParUsrBB}{default FALSE, see \code{\link{Spatial-class}} for further details}
\item{useRasterImage}{default \code{!.isSDI()} as a workaround for a problem with repeated use in Windows SDI installations; if TRUE, use \code{\link[graphics]{rasterImage}} to render the image if available; for legacy rendering set FALSE}
\item{breaks}{class breaks for coloured values}
\item{interpolate}{default FALSE, a logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing, see \code{\link[graphics]{rasterImage}}}
\item{angle}{default 0, angle of rotation (in degrees, anti-clockwise from positive x-axis, about the bottom-left corner), see \code{\link[graphics]{rasterImage}}}
\item{im}{list with components named x, y, and z, as used for \code{image}}
\item{p4}{CRS object, proj4 string}
\item{digits}{default 10, number of significant digits to use for checking equal row/column spacing}
}
\value{ \code{as.image.SpatialGridDataFrame} returns the list with
elements \code{x} and \code{y}, containing the coordinates of the cell
centres of a matrix \code{z}, containing the attribute values in matrix
form as needed by \link{image}. }

\note{ Providing \code{xcol} and \code{ycol} attributes seems obsolete,
and it is for 2D data, but it may provide opportunities for plotting
certain slices in 3D data. I haven't given this much thought yet.

\link{filled.contour} seems to misinterpret the coordinate values,
if we take the image.default manual page as the reference. }


\author{ Edzer Pebesma }

\seealso{ \link{image.default}, \link{SpatialGridDataFrame-class},
\link{levelplot} in package \code{lattice}. Function
\link[fields]{image.plot} can be used to make a legend for an image, see an example in
\url{https://stat.ethz.ch/pipermail/r-sig-geo/2007-June/002143.html}
}

\examples{
data(meuse.grid)
coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(meuse.grid) = TRUE            # promote to SpatialGridDataFrame
data(meuse)
coordinates(meuse) = c("x", "y")
image(meuse.grid["dist"], main = "Distance to river Meuse")
points(coordinates(meuse), pch = "+")
image(meuse.grid["dist"], main = "Distance to river Meuse",
 useRasterImage=TRUE)
points(coordinates(meuse), pch = "+")

# color scale:
layout(cbind(1,2), c(4,1),1)
image(meuse.grid["dist"])
imageScale(meuse.grid$dist, axis.pos=4, add.axis=FALSE)
axis(4,at=c(0,.2,.4,.8), las=2)

data(Rlogo)
d = dim(Rlogo)
cellsize = abs(c(gt[2],gt[6]))
cells.dim = c(d[1], d[2]) # c(d[2],d[1])
cellcentre.offset = c(x = gt[1] + 0.5 * cellsize[1], y = gt[4] - (d[2] - 0.5) * abs(cellsize[2]))
grid = GridTopology(cellcentre.offset, cellsize, cells.dim)
df = as.vector(Rlogo[,,1])
for (band in 2:d[3]) df = cbind(df, as.vector(Rlogo[,,band]))
df = as.data.frame(df)
names(df) = paste("band", 1:d[3], sep="")
Rlogo <- SpatialGridDataFrame(grid = grid, data = df)
summary(Rlogo)
image(Rlogo, red="band1", green="band2", blue="band3")
image(Rlogo, red="band1", green="band2", blue="band3",
 useRasterImage=FALSE)
is.na(Rlogo$band1) <- Rlogo$band1 == 255
is.na(Rlogo$band2) <- Rlogo$band2 == 255
is.na(Rlogo$band3) <- Rlogo$band3 == 255
Rlogo$i7 <- 7
image(Rlogo, "i7")
image(Rlogo, red="band1", green="band2", blue="band3", add=TRUE)
}

\keyword{spatial}
