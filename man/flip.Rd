\name{flip}
\alias{flipHorizontal}
\alias{flipVertical}
\title{ rearrange data in SpatialPointsDataFrame or SpatialGridDataFrame 
for plotting with spplot (levelplot/xyplot wrapper) }
\description{ 
rearrange SpatialPointsDataFrame for plotting with spplot or levelplot }
\usage{
flipHorizontal(x)
flipVertical(x)
}
\arguments{
\item{x}{ object of class SpatialGridDataFrame }
}

\value{ 
object of class SpatialGridDataFrame, with pixels flipped horizontally
or vertically. Note that the spatial structure is destroyed (or at least:
drastically changed).  }
\author{Michael Sumner}
\examples{
data(meuse.grid) # data frame
gridded(meuse.grid) = c("x", "y") # promotes to 
fullgrid(meuse.grid) = TRUE
d = meuse.grid["dist"]
image(d, axes=TRUE)
image(flipHorizontal(d), axes=TRUE)
image(flipVertical(d), axes=TRUE)
}
\keyword{dplot}
