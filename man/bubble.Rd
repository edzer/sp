\name{bubble}
\alias{bubble}
\title{
Create a bubble plot of spatial data
}
\description{
Create a bubble plot of spatial data, with options for bicolour
residual plots (xyplot wrapper)
}
\usage{
bubble(obj, zcol = 1, ..., fill = TRUE, maxsize = 3, do.sqrt = TRUE, pch, 
	col = c("#d01c8b", "#4dac26"), key.entries = quantile(data[,zcol]), main, 
	identify = FALSE, labels = row.names(data.frame(obj)), key.space = "right",
	scales = list(draw = FALSE), xlab = NULL, ylab = NULL, panel = panel.bubble,
	sp.layout = NULL, 
	xlim = bbexpand(bbox(obj)[1,], 0.04), 
	ylim = bbexpand(bbox(obj)[2,], 0.04))
}
\arguments{
\item{obj}{ object of, or extending, class SpatialPointsDataFrame 
or SpatialGridDataFrame, see \link{coordinates} or \link{SpatialPointsDataFrame}; 
the object knows about its spatial coordinates}
\item{zcol}{ z-variable column name, or column number after removing
spatial coordinates from x@data: 1 refers to the first non-coordinate
column }
\item{fill}{ logical; if TRUE, filled circles are plotted (pch = 16), 
else open circles (pch = 1); the pch argument overrides this }
\item{maxsize}{ \code{cex} value for largest circle }
\item{do.sqrt}{ logical; if TRUE the plotting symbol area (sqrt(diameter)) 
is proportional to the value of the z-variable; if FALSE, the symbol size
(diameter) is proportional to the z-variable }
\item{pch}{ plotting character }
\item{col}{ colours to be used; numeric vector of size two: first value
is for negative values, second for positive values. Default colors: 
5-class PiYG from colorbrewer.org. }
\item{key.entries}{ the values that will be plotted in the key; by default 
the five quantiles min, q.25, median q.75, max }
\item{main}{ main plotting title }
\item{identify}{ logical; if true, regular \code{plot} is called instead of
\code{xyplot}, and followed by a call to \code{identify()}. }
\item{labels}{ labels argument passed to \code{plot} if \code{identify}
is TRUE }
\item{...}{ arguments, passed to \code{xyplot}, or \code{plot} if 
identification is required. }
\item{key.space}{location of the key}
\item{scales}{scales argument as passed to \link{xyplot}}
\item{xlab}{x-axis label}
\item{ylab}{y-axis label}
\item{panel}{panel function used}
\item{sp.layout}{possible layout items; see \link{spplot}}
\item{xlim}{x axis limit}
\item{ylim}{y axis limit}
}
\value{
returns (or plots) the bubble plot; if \code{identify} is TRUE, returns
the indexes (row numbers) of identified points.
}
%\references{ }
\author{ Edzer Pebesma }
\seealso{\link{xyplot}, \link{mapasp}, \link{identify}}
\examples{
data(meuse)
coordinates(meuse) <- c("x", "y") # promote to SpatialPointsDataFrame
bubble(meuse, "cadmium", maxsize = 2.5, main = "cadmium concentrations (ppm)", 
    key.entries = 2^(-1:4))
bubble(meuse, "zinc", main = "zinc concentrations (ppm)",
    key.entries =  100 * 2^(0:4))
}
\keyword{dplot}
