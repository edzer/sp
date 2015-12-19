\name{read.asciigrid}
\alias{read.asciigrid}
\alias{write.asciigrid}
\title{ read/write to/from (ESRI) asciigrid format }
\description{ read/write to/from ESRI asciigrid format }
\usage{ 
read.asciigrid(fname, as.image = FALSE, plot.image = FALSE, colname = fname,
 proj4string = CRS(as.character(NA)))
write.asciigrid(x, fname, attr = 1, na.value = -9999, ...)
}

\arguments{
\item{fname}{ file name }
\item{as.image}{logical; if FALSE, a list is returned, ready to be
shown with the \code{image} command; if FALSE an object of
class \link{SpatialGridDataFrame-class} is returned }
\item{plot.image}{logical; if TRUE, an image of the map is plotted}
\item{colname}{alternative name for data column if not file name}
\item{proj4string}{A CRS object setting the projection arguments of the Spatial Grid returned}
\item{x}{ object of class \link{SpatialGridDataFrame}}
\item{attr}{attribute column; if missing, the first
column is taken; a name or a column number may be given}
\item{na.value}{numeric; value given to missing valued cells in the
resulting map}
\item{...}{ arguments passed to \link{write.table}, which is used
to write the numeric data }
}
\value{
	\code{read.asciigrid} returns the grid map read; either as
	an object of class \link{SpatialGridDataFrame-class} or, if
	as.image is TRUE, as list with components \code{x}, \code{y} and \code{z}.
}
\seealso{ \link{as.image.SpatialGridDataFrame}, \link{image} }
\author{Edzer Pebesma}
\examples{
x <- read.asciigrid(system.file("external/test.ag", package="sp")[1])
class(x)
image(x)
}
\keyword{programming}
