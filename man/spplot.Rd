\name{spplot}
\docType{methods}
\alias{spplot}
\alias{spplot.grid}
\alias{spplot.locator}
\alias{spplot.polygons}
\alias{spplot.points}
\alias{layout.north.arrow}
\alias{layout.scale.bar}
\alias{mapLegendGrob}
\alias{sp.theme}
\alias{spplot-methods}
\alias{spplot,SpatialPixelsDataFrame-method}
\alias{spplot,SpatialGridDataFrame-method}
\alias{spplot,SpatialPolygonsDataFrame-method}
\alias{spplot,SpatialLinesDataFrame-method}
\alias{spplot,SpatialPointsDataFrame-method}
\alias{spplot,SpatialMultiPointsDataFrame-method}
\alias{sppanel}
\alias{sppanel,Line-method}
\alias{sppanel,Lines-method}
\alias{sppanel,NULL-method}
\alias{sppanel,SpatialGrid-method}
\alias{sppanel,SpatialLines-method}
\alias{sppanel,SpatialPixels-method}
\alias{sppanel,SpatialPoints-method}
\alias{sppanel,SpatialPolygons-method}
\alias{sppanel,character-method}
\alias{sppanel,list-method}
\alias{panel.RgoogleMaps}
\alias{panel.ggmap}
\alias{sppanel-methods}
\alias{set_col_regions}
\alias{get_col_regions}

\title{Plot methods for spatial data with attributes}
\description{Lattice (trellis) plot methods for spatial data with attributes}
\usage{
spplot(obj, ...)
spplot.grid(obj, zcol = names(obj), ..., names.attr, scales = list(draw = FALSE),
 xlab = NULL, ylab = NULL, aspect = mapasp(obj,xlim,ylim),
 panel = panel.gridplot, sp.layout = NULL, formula, xlim = bbox(obj)[1, ],
 ylim = bbox(obj)[2, ], checkEmptyRC = TRUE, col.regions = get_col_regions())
spplot.polygons(obj, zcol = names(obj), ..., names.attr, scales = list(draw = FALSE),
 xlab = NULL, ylab = NULL, aspect = mapasp(obj,xlim,ylim),
 panel = panel.polygonsplot, sp.layout = NULL, formula, xlim = bbox(obj)[1, ],
 ylim = bbox(obj)[2, ], col.regions = get_col_regions())
spplot.points(obj, zcol = names(obj), ..., names.attr, scales = list(draw = FALSE),
 xlab = NULL, ylab = NULL, aspect = mapasp(obj,xlim,ylim),
 panel = panel.pointsplot, sp.layout = NULL, identify = FALSE, formula,
 xlim = bbexpand(bbox(obj)[1, ], 0.04), ylim = bbexpand(bbox(obj)[2, ], 0.04),
 edge.col = "transparent", colorkey = FALSE, col.regions = get_col_regions())
mapLegendGrob(obj, widths = unit(1, "cm"), heights = unit(1, "cm"),
	fill = "black", just = "right")
sp.theme(set = FALSE, regions = list(col = bpy.colors(100)), ...)
layout.north.arrow(type = 1)
layout.scale.bar(height = 0.05)
spplot.locator(n = 512, type = "n", ...)
set_col_regions(value)
get_col_regions()
}
\arguments{
\item{obj}{ object of class extending \link{Spatial-class}} 
\item{zcol}{ character; attribute name(s) or column number(s) in attribute table }
\item{names.attr}{ names to use in panel, if different from zcol names }
\item{scales}{ scales argument to be passed to Lattice plots; use
\code{list(draw = TRUE)} to draw axes scales; see \link{xyplot} for full options }
\item{...}{ other arguments passed to \link{levelplot} (grids, polygons) or 
\link{xyplot} (points) }
\item{xlab}{label for x-axis }
\item{ylab}{label for y-axis }
\item{aspect}{aspect ratio for spatial axes; defaults to "iso" (one unit
on the x-axis equals one unit on the y-axis) but may be set to more
suitable values if the data are e.g. if coordinates are latitude/longitude }
\item{panel}{ depending on the class of obj, \link{panel.polygonsplot} (for
polygons or lines), \link{panel.gridplot} (grids) or \link{panel.pointsplot}
(points) is used; for further control custom panel functions can be
supplied that call one of these panel functions, but do read below how the
argument \code{sp.layout} may help }
\item{sp.layout}{NULL or list; see notes below}
\item{identify}{ if not FALSE, identify plotted objects (currently only working
for points plots). Labels for identification are the row.names of the
attribute table \code{row.names(as.data.frame(obj))}. If TRUE, identify
on panel \code{(1,1)}; for identifying on panel \code{i,j}, pass the
value \code{c(i,j)}}
\item{formula}{optional; may be useful to plot a transformed value. Defaults
to \code{z~x+y} for single and \code{z~x+y|name} for multiple attributes; use
e.g. \code{exp(x)~x+y|name} to plot the exponent of the z-variable}
\item{xlim}{ numeric; x-axis limits }
\item{ylim}{ numeric; y-axis limits }
\item{edge.col}{ color of symbol edge }
\item{colorkey}{ if FALSE, use symbol key; if TRUE, use continuous, levelplot-like colorkey; if list, follow syntax of argument \code{colorkey} in \link[lattice]{levelplot} (see below for an example) }
\item{widths}{width of grob}
\item{heights}{heights of grob}
\item{fill}{fill color of grob}
\item{just}{grob placement justification}
\item{set}{ logical; if TRUE, trellis.par.set is called, else a list is returned
that can be passed to trellis.par.set() }
\item{regions}{ color ramp for the theme }
\item{height}{height of scale bar; width is 1.0}
\item{n}{see locator}
\item{type}{see locator}
\item{checkEmptyRC}{logical; if TRUE, a check is done to see if
empty rows or columns are present, and need to be taken care of. Setting
to FALSE may improve speed. }
\item{col.regions}{ vector with fill colours; in case the variable
to be plotted is a factor, this vector should have length equal to the
number of factor levels } 
\item{value}{ vector with color values, default for \code{col.regions}}
}
\value{
spplot returns a lattice plot of class "trellis", if you fail to "see" it,
explicitly call \code{print(spplot(...))}. If \code{identify} is TRUE,
the plot is plotted and the return value is a vector with row names of
the selected points.

\code{spplot.locator} returns a matrix with identified point locations;
use \code{trellis.focus} first to focus on a given panel.

\code{get_col_regions} returns the default value for \code{col.regions}
}

\section{Methods}{
\describe{
\item{obj = "SpatialPixelsDataFrame"}{ see \link{spplot} }
\item{obj = "SpatialGridDataFrame"}{ see \link{spplot} }
\item{obj = "SpatialPolygonsDataFrame"}{ see \link{spplot} }
\item{obj = "SpatialLinesDataFrame"}{ see \link{spplot} }
\item{obj = "SpatialPointsDataFrame"}{ see \link{spplot} }
}}

\note{
Missing values in the attributes are (currently) not allowed.

\code{spplot.grid}, \code{spplot.polygons} and \code{spplot.points} are S4
methods for \code{spplot}; see \link{spplot-methods}.

Useful arguments that can be passed as \code{...} are: 
\describe{
\item{\code{layout}}{integer; for the layout of panels (cols,rows)}
\item{\code{pretty}}{logical; choose colour breaks at pretty numbers?}
\item{\code{at}}{specify at which values colours change}
\item{\code{as.table}}{logical; start drawing panels upper-left instead
of lower-left}
\item{\code{page}}{to add marks to each plotted page}
}

for useful values see the appropriate documentation of \link{xyplot}
(in case of points), and \link{levelplot} (otherwise).

If \code{obj} is of SpatialPointsDataFrame, the following options are
useful to pass:
\describe{
\item{\code{key.space}}{ character: "bottom", "right", "left" or "right" to denote
key location, or list: see argument key in the help for \link{xyplot} what
the options are}
\item{\code{legendEntries}}{ character; array with key legend (text) entries; 
suitable defaults obtained from data }
\item{\code{cuts}}{number of cuts, or, for objects of class \link{SpatialPointsDataFrame} only, the actual cuts to use}
\item{\code{do.log}}{logical; if TRUE use log-linear scale to divide range in equal cuts,
else use a linear scale if \code{cuts} is only number of cuts }
\item{\code{pch}}{ integer; plotting character to use; defaults to 16 if fill is TRUE, else 1 }
\item{\code{cex}}{numeric; character expansion, proportional to default value of 1}
\item{\code{fill}}{ logical; use filled circles? }
}

\code{layout.north.arrow} and \code{layout.scale.bar} can be used
to set a north arrow or scale bar.

The \code{sp.layout} argument is either a single layout item, or a list
with one or more layout items. A layout item is one of
\itemize{
\item{a list with one or more \code{Spatial*} objects, along with
style arguments like \code{col}, \code{lty}, \code{pch}, \code{fill} etc.}
\item{a list with its first argument the layout function or
the name of the layout function to be called: \code{sp.points} for
SpatialPoints, \code{sp.polygons} for SpatialPolygons object, \code{sp.lines}
for a SpatialLines object, and \code{sp.text} for text to place. The
second argument contains the object (or text) to be plotted; remaining
arguments are passed to the corresponding \code{panel.*} functions.}}

The order of items in \code{sp.layout} matters; objects are drawn in the
order they appear. With respect to \code{obj}, default plot order and
precedence of \code{sp.layout} items is as follows: for points and
lines, \code{sp.layout} items are drawn over (after) \code{obj}; for
grids and polygons, \code{sp.layout} items are drawn behind (before)
\code{obj}.  Transparency may further help making multiple things
visible. Adding a \code{first} argument to a layout item overrides its
default plotting order with respect to \code{obj}:

Special control elements of \code{sp.layout} items:
\describe{
\item{\code{first}}{logical; should the layout item be drawn before
the \code{obj} (TRUE), or after (FALSE)? This overrides
the default order (points and lines in front, polygons and grids behind). }
\item{\code{which}}{integer; controls to which panel a layout
item should be added. If \code{which} is present in the main,
top-level list it applies to all layout items; in sub-lists with
layout items it denotes the (set of) panel(s) in which the layout
item should be drawn.  Without a \code{which} item, layout items
are drawn in each panel.}
}

\code{sp.theme} returns a lattice theme; use, after loading package
\code{lattice}, the command \code{trellis.par.set(sp.theme())}
after a device is opened or changed to make this work. Currently, this only
sets the colors to \link{bpy.colors}.

If the attributes to be plotted are of type factor, spplot tries
to create a legend that reflects this. In this case, the color ramp
passed needs to be of the same length as the number of factor levels.
The factor levels are derived from the first map; subsequent factors
with different factor levels result in an error.

}

\references{
\url{https://edzer.github.io/sp/}
}
\seealso{\link[lattice]{xyplot}, \link[lattice]{levelplot},
\link[lattice]{panel.identify} to identify objects }
\author{
Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}
}
\examples{
library(lattice)
trellis.par.set(sp.theme()) # sets bpy.colors() ramp
demo(meuse, ask = FALSE, echo = FALSE)
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(181300,329800), 
	scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800), 
	scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")

spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5), col.regions= "black", 
	pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse, c("zinc", "lead"), sp.layout=list(l2,l3,l4,l5, which = 2),
	key.space=list(x=0.1,y=.95,corner=c(0,1)))
# plotting factors:
meuse$f = factor(sample(letters[6:10], 155, replace=TRUE),levels=letters[1:10])
meuse$g = factor(sample(letters[1:5], 155, replace=TRUE),levels=letters[1:10])
spplot(meuse, c("f","g"), col.regions=bpy.colors(10))

if (require(RColorBrewer)) {
	spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5),
		col.regions=brewer.pal(3, "Set1"))
}

meuse.grid$g = factor(sample(letters[1:5], 3103, replace=TRUE),
	levels=letters[1:10])
meuse.grid$f = factor(sample(letters[6:10], 3103, replace=TRUE),
	levels=letters[1:10])
spplot(meuse.grid, c("f","g"), col.regions=bpy.colors(10))

# example modifying colorkey for points:
spplot(meuse["dist"], colorkey = list(
	right = list( # see ?levelplot in package trellis, argument colorkey:
		fun = draw.colorkey, 
		args = list(
			key = list(
				at = seq(0, 1, .1), # colour breaks
				col = bpy.colors(11), # colours
				labels = list(
					at = c(0, .2, .4, .6, .8, 1), 
					labels = c("0x", "20x", "40x", "60x", "80x", "100x")
				)
			)
		)
	)
))
l6 = list(meuse.grid["dist"], col = grey(seq(.5,.9,length.out=10)))
spplot(meuse, c("zinc", "lead"), sp.layout = l6)
spplot(meuse, c("zinc", "lead"), 
	sp.layout = list(meuse.grid, meuse.riv, col = 'grey'))

#  Custom legend placement, taken from 
# http://stackoverflow.com/questions/29344692/custom-placement-of-spplot-legend-in-the-map
s <- spplot(meuse.grid[,'dist'], colorkey = list(space = "left", height = 0.4))

args <- s$legend$left$args$key

## Prepare list of arguments needed by `legend=` argument (as described in ?xyplot)
library(lattice) # draw.colorkey
legendArgs <- list(fun = draw.colorkey,
                   args = list(key = args),
                   corner = c(0.05,.75))

## Call spplot() again, this time passing in to legend the arguments
## needed to print a color key
spplot(meuse.grid[,'dist'], colorkey = FALSE,
       legend = list(inside = legendArgs))
}
\keyword{dplot}
