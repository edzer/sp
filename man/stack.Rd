\name{stack}
\alias{spmap.to.lev}
\alias{stack.SpatialPointsDataFrame}
\alias{stack.SpatialGridDataFrame}
\title{ rearrange data in SpatialPointsDataFrame or SpatialGridDataFrame 
for plotting with spplot (levelplot/xyplot wrapper) }
\description{ 
rearrange SpatialPointsDataFrame for plotting with spplot or levelplot }
\usage{
spmap.to.lev(data, zcol = 1:n, n = 2, names.attr)
\method{stack}{SpatialPointsDataFrame}(x, select, ...)
\method{stack}{SpatialGridDataFrame}(x, select, ...)
}
\arguments{
\item{data}{ object of class (or extending) SpatialPointsDataFrame or
SpatialGridDataFrame }
\item{zcol}{ z-coordinate column name(s), or a column number (range) (after
removing the spatial coordinate columns: 1 refers to the first non-coordinate
column, etc. ) }
\item{names.attr}{names of the set of z-columns (these names
will appear in the plot); if omitted, column names of \code{zcol} }
\item{n}{ number of columns to be stacked }
\item{x}{ same as \code{data}}
\item{select}{ same as \code{zcol}}
\item{...}{ ignored }
}

\value{ 
\code{spmap.to.lev} returns a data frame with the following elements:
\item{x}{ x-coordinate for each row}
\item{y}{ y-coordinate for each row} 
\item{z}{ column vector with each of the elements in columns \code{zcol}
of \code{data} stacked } 
\item{name}{ factor; name of each of the stacked \code{z} columns } 

\code{stack} is an S3 method: it return a data.frame with a column
\code{values} that has the stacked coordinates and attributes, and a
column \code{ind} that indicates the variable stacked; it also
replicates the coordinates. }

\seealso{ \link{spplot}, \link{levelplot} in package \code{lattice}, and \link{stack}}
\examples{
library(lattice)
data(meuse.grid) # data frame
coordinates(meuse.grid) = c("x", "y") # promotes to SpatialPointsDataFrame
meuse.grid[["idist"]] = 1 - meuse.grid[["dist"]] # add variable
# the following is made much easier by spplot:
levelplot(z~x+y|name, spmap.to.lev(meuse.grid, z=c("dist","idist"), names.attr =
	c("distance", "inverse of distance")), aspect = "iso")
levelplot(values~x+y|ind, as.data.frame(stack(meuse.grid)),aspect = "iso")
gridded(meuse.grid) = TRUE
levelplot(z~x+y|name, spmap.to.lev(meuse.grid, z=c("dist","idist"), names.attr =
	c("distance", "inverse of distance")), aspect = "iso")
levelplot(values~x+y|ind, as.data.frame(stack(meuse.grid)), asp = "iso")

}
\keyword{dplot}
