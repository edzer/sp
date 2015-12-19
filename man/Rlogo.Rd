\name{Rlogo}
\alias{Rlogo}
\alias{gt}
\docType{data}
\title{Rlogo jpeg image}
\description{
  Rlogo jpeg image data as imported by \code{getRasterData} in the rgdal package
}
\usage{data(Rlogo)}
\format{
  The format is:
 int [1:101, 1:77, 1:3] 255 255 255 255 255 255 255 255 255 255 ...
}

\examples{
\dontrun{
library(rgdal)
logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
x <- GDAL.open(logo)
gt = .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
data <- getRasterData(x)
GDAL.close(x)
}
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
spplot(Rlogo, zcol=1:3, names.attr=c("red","green","blue"), 
	col.regions=grey(0:100/100),
	main="example of three-layer (RGB) raster image", as.table=TRUE)
}
\keyword{datasets}
