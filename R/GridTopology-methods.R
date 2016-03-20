GridTopology = function(cellcentre.offset, cellsize, cells.dim) {
	new("GridTopology",
		cellcentre.offset = cellcentre.offset,
		cellsize = cellsize,
		cells.dim = as.integer(cells.dim))
}

# setMethod("show", "GridTopology", function(object) summary(object))

as.data.frame.GridTopology = function(x, row.names, optional, ...) data.frame(
		cellcentre.offset = x@cellcentre.offset,
		cellsize = x@cellsize,
		cells.dim = x@cells.dim
	)

setAs("GridTopology", "data.frame", function(from) as.data.frame.GridTopology(from))

setMethod("coordinates", "GridTopology", function(obj) {
	cc = do.call(expand.grid, coordinatevalues(obj))
#	as.matrix(sapply(cc, as.numeric))
# dropping dimension for single cell grid
	do.call(cbind, lapply(cc, as.numeric))
})

coordnamesGT = function(x, value) {
	names(x@cellcentre.offset) = value
	names(x@cellsize) = value
	names(x@cells.dim) = value
	x
}

setReplaceMethod("coordnames", 
	signature(x = "GridTopology", value = "character"), coordnamesGT)

coordinatevalues = function(obj) {
	if (!is(obj, "GridTopology"))
		stop("function only works for objects of class or extending GridTopology")
	ret = list()
	for (i in seq(along=obj@cells.dim)) {
		if (i == 2) # y-axis is the exception--starting at top of map, and decreasing:
			ret[[i]] = obj@cellcentre.offset[i] + 
				obj@cellsize[i] * ((obj@cells.dim[i] - 1):0)
		else
			ret[[i]] = obj@cellcentre.offset[i] + 
				obj@cellsize[i] * (0:(obj@cells.dim[i] - 1))
	}
	ns = names(obj@cellcentre.offset)
	if (is.null(ns))
		ns = paste("s", 1:length(ret), sep = "") #dimnames(obj@bbox)[[1]]
	names(ret) = ns
	ret
}

points2grid = function(points, tolerance=sqrt(.Machine$double.eps), 
		round = NULL) {
	# work out grid topology from points
	n = dimensions(points)
	ret = new("GridTopology", 
		cellcentre.offset = numeric(n),
		cellsize = numeric(n),
		cells.dim = as.integer(rep(1,n)))
	cc = coordinates(points)
	nr <- nrow(cc)
	for (i in 1:n) { # loop over x, y, and possibly z
		x = cc[, i]
		sux = sort(unique(x))
    	difx = diff(sux)
		if (length(difx) == 0) {
			warning(paste("cell size from constant coordinate", i,
				"possibly taken from other coordinate"))
			ret@cellsize[i] = NA
			ret@cellcentre.offset[i] = min(sux)
    		ret@cells.dim[i] = as.integer(1)
		} else {
			ru.difx = range(unique(difx)) # min to max x coord leaps
			# next, find cases where some of the differences are
			# VERY close to zero, and remove these:
			if (ru.difx[1] / ru.difx[2] < tolerance) {
				difx = difx[difx > ru.difx[2] * tolerance]
				ru.difx = range(unique(difx)) # reset
			}
			err1 = diff(ru.difx) #?? /max(range(abs(sux))) # (max-min)/max(abs(x))
			if (err1 > tolerance) { 
				xx = ru.difx / min(ru.difx)
				err2 = max(abs(floor(xx) - xx)) # is it an integer multiple?
				if (err2 > tolerance) {
					cat(paste("suggested tolerance minimum:", signif(err2, 6), 
						"\n"))
					stop(paste("dimension", i,": coordinate intervals are not constant"))
				} else {
			    	difx = difx[difx < ru.difx[1] + tolerance]
			    	warning(paste("grid has empty column/rows in dimension", i))
				}
			}
			ret@cellsize[i] = mean(difx)
			ret@cellcentre.offset[i] = min(sux)
    		ret@cells.dim[i] = as.integer(round(diff(range(sux))/ret@cellsize[i]) + 1) 
				#was: length(sux), but this will not cope with empty rows.
			if (ret@cells.dim[i] > nr) 
				warning(paste("grid topology may be corrupt in dimension", i))
		}
	}
	cs = ret@cellsize
	if (any(is.na(cs))) {
		if (all(is.na(cs)))
			stop("cannot derive grid parameters from a single point!")
		ret@cellsize[is.na(cs)] = cs[!is.na(cs)][1]
	}
	nm = dimnames(cc)[[2]]
	names(ret@cellsize) = nm
	names(ret@cellcentre.offset) = nm
	names(ret@cells.dim) = nm
	ret
}

.NumberOfCells = function(x) {
	if (!is(x, "GridTopology"))
		stop(".NumberOfCells only works on objects of class GridTopology")
	prod(x@cells.dim)
}

print.GridTopology = function(x, ...) {
	res = data.frame(rbind(x@cellcentre.offset, x@cellsize, as.numeric(x@cells.dim)))
	rownames(res) = c("cellcentre.offset", "cellsize", "cells.dim")
	if (!is.null(names(x@cellcentre.offset)))
		names(res) = names(x@cellcentre.offset)
	print(res)
	invisible(res)
}
setMethod("show", "GridTopology", function(object) print.GridTopology(object))

summ.GridTopology = function(object, ...) {
	ret = list()
	ret[["values"]] = gridparameters(object)
	class(ret) = "summary.GridTopology"
	ret
}
setMethod("summary", "GridTopology", summ.GridTopology)

print.summary.GridTopology = function(x, ...) {
	cat("Grid topology:\n")
	print(x$values)
	invisible(x)
}

# make a SpatialPolygons from a GridTopology - NERSC query

as.SpatialPolygons.GridTopology <- function(grd, proj4string=CRS(as.character(NA)))
{
	coordnam = names(grd@cellcentre.offset)
	grd_crds <- coordinates(grd)
	IDs <- IDvaluesGridTopology(grd)
	nPolygons <- nrow(grd_crds)
	cS <- grd@cellsize
	cS2 <- cS/2
	cS2x <- cS2[1]
	cS2y <- cS2[2]
	Srl <- vector(mode="list", length=nPolygons)
	for (i in 1:nPolygons) {
		xi <- grd_crds[i,1]
		yi <- grd_crds[i,2]
		x <- c(xi-cS2x, xi-cS2x, xi+cS2x, xi+cS2x, xi-cS2x)
		y <- c(yi-cS2y, yi+cS2y, yi+cS2y, yi-cS2y, yi-cS2y)
		coords = cbind(x, y)
		if (!is.null(coordnam))
			dimnames(coords) = list(NULL, coordnam)
		else
			rownames(coords) = NULL
		Srl[[i]] <- Polygons(list(Polygon(coords = coords)), ID = IDs[i])
		comment(Srl[[i]]) <- "0"
	}
	res <- SpatialPolygons(Srl, proj4string = proj4string)
	# address https://github.com/edzer/sp/issues/3:
	if (!is.null(coordnam))
		dimnames(res@bbox)[[1]] = coordnam
	res
}
setAs("GridTopology", "SpatialPolygons", function(from)
	as.SpatialPolygons.GridTopology(from))

# mostly copied from coordinates() for GridTopology, 
# makes IDs "c(i)r(j)" matching the coordinates
# used with SpatialRing-ed grids for the data frame rowname()

IDvaluesGridTopology <- function(obj) {
	if (!is(obj, "GridTopology"))
		stop("function only works for objects of class or extending GridTopology")
	cc <- getGridIndex(coordinates(obj), obj)
	res <- as.matrix(sapply(cc, as.integer))
	paste("g", res, sep="")
}

