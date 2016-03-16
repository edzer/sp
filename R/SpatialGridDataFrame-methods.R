SpatialPixelsDataFrame = function(points, data, 
		tolerance = sqrt(.Machine$double.eps), 
		proj4string = CRS(as.character(NA)), round = NULL, grid = NULL) {
	if (is.null(points))
		stop("points argument is NULL")
	if (is(points, "SpatialPixels") && is.null(grid))
		grid = points@grid
	if (!is(points, "SpatialPoints"))
		points = SpatialPoints(points, proj4string = proj4string)
	points = SpatialPixels(points, tolerance = tolerance, round = round,	
		grid = grid)
	new("SpatialPixelsDataFrame", points, data = data)
}

SpatialGridDataFrame = function(grid, data, proj4string = CRS(as.character(NA))) {
	if (!is(grid, "SpatialGrid"))
		grid = SpatialGrid(grid, proj4string)
	new("SpatialGridDataFrame", grid, data = data)
}

setMethod("addAttrToGeom", signature(x = "SpatialPixels", y = "data.frame"),
	function(x, y, match.ID, ...)
		SpatialPixelsDataFrame(geometry(x), y, ...)
)

setMethod("addAttrToGeom", signature(x = "SpatialGrid", y = "data.frame"),
	function(x, y, match.ID, ...)
		SpatialGridDataFrame(geometry(x), y, ...)
)

as.SPixDF.SGDF = function(from) {
   	data = list()
   	n = .NumberOfCells(from@grid)
   	for (i in seq(along = from@data)) {
		v = vector(mode(from@data[[i]]), n)
      	if (is.factor(from@data[[i]]))
			v = factor(rep(NA, n), levels = levels(from@data[[i]]))
		else
			v[-from@grid.index] = NA
		v[from@grid.index] = from@data[[i]]
		data[[i]] = v
   	}
   	data = data.frame(data, stringsAsFactors = FALSE)
   	names(data) = names(from@data)
	SpatialGridDataFrame(from@grid, data, CRS(proj4string(from)))
}
#setAs("SpatialPixelsDataFrame", "SpatialGridDataFrame", as.SPixDF.SGDF)

# Jon Skoien, Apr 18, 2013:
# Version where from@data is first added to data, then the columns with
# factors are correctly added in a second step. By far the fastest when 
# there are few factor columns.
pix2grid = function(from) {
	n = .NumberOfCells(from@grid)
	data = data.frame(matrix(nrow = n, ncol = ncol(from@data)))
	data[from@grid.index,] = from@data # takes care of character columns
	names(data) = names(from@data)
# Which columns have factors
	fids = which(sapply(from@data, is.factor))
	if (length(fids) > 0) {
		for (fi in fids) {
			v = vector(mode(from@data[[fi]]), n)
			v = factor(rep(NA, n), levels = levels(from@data[[fi]]))
			v[from@grid.index] = from@data[[fi]]
			data[,fi] = v
		}
	}
	SpatialGridDataFrame(from@grid, data, CRS(proj4string(from)))
}
setAs("SpatialPixelsDataFrame", "SpatialGridDataFrame", pix2grid)

as.SGDF.SPixDF = function(from) { 
	# find rows with only NA's in attribute table:
	sel = apply(sapply(from@data, is.na), 1, function(x) !all(x))
	#sel = TRUE
	if (!any(sel)) {
		warning("complete map seems to be NA's -- no selection was made")
		sel = rep(TRUE, length(sel))
	}
   	#SpatialPixelsDataFrame(points = coordinates(from)[sel,], 
	#	data = from@data[sel,,drop=FALSE], proj4string = CRS(proj4string(from)))
	new("SpatialPixelsDataFrame", 
		new("SpatialPixels", 
			new("SpatialPoints", coords = coordinates(from)[sel,,drop=FALSE], 
				bbox = from@bbox, proj4string = from@proj4string),
			grid = from@grid,
			grid.index = which(sel)),
		data = from@data[sel,,drop=FALSE])
}
setAs("SpatialGridDataFrame", "SpatialPixelsDataFrame", as.SGDF.SPixDF)
setAs("SpatialGridDataFrame", "SpatialPointsDataFrame", 
	function(from) as(as(from, "SpatialPixelsDataFrame"), "SpatialPointsDataFrame"))

setMethod("coordinates", "SpatialPixelsDataFrame", 
	function(obj) coordinates(as(obj, "SpatialPixels")))

setMethod("coordinates", "SpatialGridDataFrame", 
	function(obj) coordinates(as(obj, "SpatialGrid")))

row.names.SpatialGridDataFrame <- function(x) {
	#warning("row.names order might be wrong!")
	#1:prod(x@grid@cells.dim)
	row.names(x@data)
}

as.SpPixDF.SpPoiDF = function(from)
	new("SpatialPointsDataFrame", as(from, "SpatialPoints"), 
		data = from@data, coords.nrs = from@coords.nrs)
setAs("SpatialPixelsDataFrame", "SpatialPointsDataFrame", as.SpPixDF.SpPoiDF)

as.SpatialPolygonsDataFrame.SpatialPixelsDataFrame = function(from) {
        df <- from@data
        SP <- as(from, "SpatialPolygons")
        row.names(df) <- row.names(SP)
	SpatialPolygonsDataFrame(SP, df)
}

setAs("SpatialPixelsDataFrame", "SpatialPolygonsDataFrame",
	as.SpatialPolygonsDataFrame.SpatialPixelsDataFrame)

as.matrix.SpatialPixelsDataFrame = function(x, ...) {
	# fullgrid(x) = TRUE
	x = as(x, "SpatialGridDataFrame")
	as.matrix(x, ...)
}

as.array.SpatialGridDataFrame = function(x,...) {
    d = gridparameters(x)$cells.dim
    if (ncol(x@data) > 1)
        d = c(d, ncol(x@data))
    array(do.call(c, x@data), dim = d)
}
setAs("SpatialGridDataFrame", "array", function(from) 
	as.array.SpatialGridDataFrame(from))

setAs("SpatialPixelsDataFrame", "array", function(from) 
	as(as(from, "SpatialGridDataFrame"), "array"))

as.matrix.SpatialGridDataFrame = function(x, ..., byrow = FALSE) {
	if (ncol(x@data) > 1)
		warning(
		"as.matrix.SpatialGridDataFrame uses first column;\n use subset or [] for other columns")
	if (byrow)
		matrix(x@data[[1]], x@grid@cells.dim[2], x@grid@cells.dim[1], byrow=byrow)
	else
		matrix(x@data[[1]], x@grid@cells.dim[1], x@grid@cells.dim[2], byrow=byrow)
}

setAs("SpatialPixelsDataFrame", "matrix", function(from) 
	as.matrix.SpatialPixelsDataFrame(from))
setAs("SpatialGridDataFrame", "matrix", function(from) 
	as.matrix.SpatialGridDataFrame(from))

as.data.frame.SpatialPixelsDataFrame = function(x, row.names, optional, ...)
	as.data.frame(as(x, "SpatialPointsDataFrame"))

as.data.frame.SpatialGridDataFrame = function(x, row.names, optional, ...)
	as.data.frame(as(x, "SpatialPixelsDataFrame"))

setAs("SpatialPixelsDataFrame", "data.frame", function(from)
	as.data.frame.SpatialPixelsDataFrame(from))
setAs("SpatialGridDataFrame", "data.frame", function(from)
	as.data.frame.SpatialGridDataFrame(from))

setMethod("[", "SpatialPixelsDataFrame", function(x, i, j, ... , drop = FALSE) {
	grid = x@grid
	x = as(x, "SpatialPointsDataFrame")
	missing.i = missing(i)
	missing.j = missing(j)
	nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
	if (missing.i && missing.j) {
		i = TRUE
		j = TRUE
	} else if (missing.j && !missing.i) { 
		if (nargs == 2) {
			j = i
			i = TRUE
		} else {
			j = TRUE
		}
	} else if (missing.i && !missing.j)
		i = TRUE
	if (is.matrix(i))
		stop("matrix argument not supported in SpatialPointsDataFrame selection")
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (any(is.na(i))) 
		stop("NAs not permitted in row index")
	x@coords = x@coords[i, , drop = FALSE]
	if (nrow(x@coords))
		x@bbox = .bboxCoords(x@coords)
	x@data = x@data[i, j, ..., drop = FALSE]
	if (drop)
		gridded(x) = TRUE
	else
		gridded(x) = list(TRUE, grid)
	x
})
#setMethod("[", "SpatialPixelsDataFrame", subs.SpatialPixelsDataFrame)

subs.SpatialGridDataFrame <- function(x, i, j, ... , drop = FALSE) {
	n.args = nargs()
	dots = list(...)
	if (drop)
		stop("argument drop needs to be FALSE")
	missing.i = missing(i)
	missing.j = missing(j)
	if (length(dots) > 0) {
		missing.k = FALSE
		k = dots[[1]]
	} else
		missing.k = TRUE
	if (missing.i && missing.j && missing.k)
		return(x)
	grd = x@grid

	if (missing.k) {
		k = TRUE
		if (missing.j && n.args != 3) { # not like : x[i,] but x[i]
			x@data = x@data[ , i, drop = FALSE]
			return(x)
		}
	} else if (missing.j && n.args == 2) {
		x@data = x@data[ , k, drop = FALSE]
		return(x)
	} 
	if (missing.i)
		rows = 1:grd@cells.dim[2]
	else { # we have an i
		if (is(i, "Spatial"))
			i = !is.na(over(x, geometry(i)))
		if (is.integer(i)) {
			if ((length(i) > grd@cells.dim[2] && length(i) < nrow(x@data))
					|| max(i) > grd@cells.dim[2]) {
				if (all(i < 0)) {
					i = -i
					negate = TRUE
				} else
					negate = FALSE
				i = (1:nrow(x@data)) %in% i
				if (negate)
					i = !i
			}
		}
		if (length(i) == nrow(x@data)) {
			if (!missing.j)
				x@data = x@data[j]
			x@data = data.frame(lapply(x@data, function(C) { C[!i] = NA; C }))
			return(x)
		}
		rows = i
	}
	if (missing.j)
		cols = 1:grd@cells.dim[1]
	else
		cols = j
	idx = 1:prod(grd@cells.dim[1:2])
	m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[rows,cols]
	idx = as.vector(m) # t(m)?
	if (any(is.na(idx)))
		stop("NAs not permitted in index")
	if (length(idx) == 0) {
		x@data = x@data[,k,drop=FALSE]
		x@data[] = NA
		return(x)
	} 
	pts = SpatialPoints(coordinates(x)[idx,,drop=FALSE], CRS(proj4string(x)))
	if (length(idx) == 1)
		SpatialPointsDataFrame(pts, x@data[idx, k, drop = FALSE])
	else {
		res = SpatialPixelsDataFrame(SpatialPixels(pts), x@data[idx, k, drop = FALSE])
		as(res, "SpatialGridDataFrame")
	}
}
setMethod("[", "SpatialGridDataFrame", subs.SpatialGridDataFrame)

cbind.SpatialGridDataFrame = function(...) { 
	stop.ifnot.equal = function(a, b) {
		res = all.equal(getGridTopology(a), getGridTopology(b))
		if (!is.logical(res) || !res)
			stop("topology is not equal")
	}
	grds = list(...)
	ngrds = length(grds)
	if (ngrds < 1)
		stop("no arguments supplied")
	if (ngrds == 1)
		return(grds[[1]])
	# verify matching topology:
	sapply(grds[2:ngrds], function(x) stop.ifnot.equal(x, grds[[1]]))
	gr = grds[[1]]
	gr@data = do.call(cbind, lapply(grds, function(x) x@data))
	#for (i in 2:ngrds)
	#	gr@data = cbind(gr@data, grds[[i]]@data)
	proj4string(gr) = CRS(proj4string(grds[[1]]))
	gr
}

print.SpatialPixelsDataFrame = function(x, ...) {
	cat("Object of class SpatialPixelsDataFrame\n")
	print(as(x, "SpatialPixels"))
	if (length(x) > 0) {
		cat("\n")
		cat("Data summary:\n")
		if (ncol(x@data) > 0)
			print(summary(x@data))
	}
	invisible(x)
}
setMethod("show", "SpatialPixelsDataFrame", 
	function(object) print.SpatialPixelsDataFrame(object))

print.SpatialGridDataFrame = function(x, ...) {
	cat("Object of class SpatialGridDataFrame\n")
	print(as(x, "SpatialGrid"))
	if (length(x) > 0) {
		cat("\n")
		cat("Data summary:\n")
        	if (ncol(x@data) > 1)
                	sobj = summary(x@data)
            	else sobj = summary(x@data[[1]])
		print(sobj)
	}
	invisible(x)
}
setMethod("show", "SpatialGridDataFrame", 
	function(object) print.SpatialGridDataFrame(object))

names.SpatialPixelsDataFrame = function(x) names(x@data)
names.SpatialGridDataFrame = function(x) names(x@data)

checkNames = function(x) { 
	if (!identical(x, make.names(x)))
		warning("attempt to set invalid names: this may lead to problems later on. See ?make.names")
}
"names<-.SpatialPixelsDataFrame" = function(x,value) { checkNames(value); names(x@data) = value; x }
"names<-.SpatialGridDataFrame" = function(x,value) { checkNames(value); names(x@data) = value; x }

dim.SpatialPixelsDataFrame = function(x) dim(x@data)


dim.SpatialGridDataFrame = function(x) dim(x@data)

setMethod("split", "SpatialPixelsDataFrame", split.data.frame)

setMethod("geometry", "SpatialGridDataFrame",
	function(obj) as(obj, "SpatialGrid"))

setMethod("geometry", "SpatialPixelsDataFrame",
	function(obj) as(obj, "SpatialPixels"))

setAs("SpatialGridDataFrame", "SpatialPolygonsDataFrame",
	function(from) {
		fullgrid(from) = FALSE
		as(from, "SpatialPolygonsDataFrame")
	}
)

length.SpatialPixelsDataFrame = function(x) { nrow(x@coords) }
length.SpatialGridDataFrame = function(x) { .NumberOfCells(x@grid) }

# http://menugget.blogspot.de/2013/12/new-version-of-imagescale-function.html
#This function creates a color scale for use with the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "axis.pos" argument
#defines the side of the axis. The "add.axis" argument defines
#whether the axis is added (default: TRUE)or not (FALSE).
image.scale <- function(z, zlim, col = heat.colors(12), breaks, axis.pos=1, add.axis = TRUE, 
		at = NULL, shrink = 0, ...) {
	stopifnot(!is.factor(z))
	if (!missing(breaks) && length(breaks) != (length(col) + 1))
		stop("must have one more break than colour")
	if (missing(zlim))
		zlim <- range(z, na.rm=TRUE)
	if (missing(breaks))
		breaks <- seq(zlim[1], zlim[2], length.out = length(col) + 1)
	Shrink = function(r, s) {
		w = diff(r)
		c(r[1] - 0.5 * s * w, r[2] + 0.5 * s * w)
	}
	if (axis.pos %in% c(1,3)) {
		ylim <- c(0, 1)
		xlim <- Shrink(range(breaks), shrink)
	}
	if (axis.pos %in% c(2,4)) {
		ylim <- Shrink(range(breaks), shrink)
		xlim <- c(0, 1)
	}
	poly <- vector(mode="list", length(col))
	for (i in seq(poly))
		poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	plot(1,1,t="n", ylim = ylim, xlim = xlim, axes = FALSE, 
		xlab = "", ylab = "", xaxs = "i", yaxs = "i", ...)  
	for(i in seq(poly)) {
		if (axis.pos %in% c(1,3))
			polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
		if (axis.pos %in% c(2,4))
			polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
	}
	if (shrink > 0) {
		if (is.null(at))
			at = pretty(breaks)
		b = c(breaks[1], breaks[length(breaks)])
		if (axis.pos %in% c(1,3))
			lines(y = c(0,1,1,0,0), x = c(b[1],b[1],b[2],b[2],b[1]))
		if (axis.pos %in% c(2,4))
			lines(x = c(0,1,1,0,0), y = c(b[1],b[1],b[2],b[2],b[1]))
	} else
		box()
	if (add.axis) 
		axis(axis.pos, at)
}

image.scale.factor <- function(z, col = heat.colors(nlevels(z)), axis.pos = 1, scale.frac = 0.3, 
		scale.n = 15, ...) {
	stopifnot(is.factor(z))
	stopifnot(axis.pos %in% c(1,4))
	frc = scale.frac
	stre = scale.n
	plot(1, 1, t="n", ylim = c(0,1), xlim = c(0,1), axes = FALSE, 
		xlab = "", ylab = "", xaxs = "i", yaxs = "i", ...)  
	n = nlevels(z)
	if (n != length(col))
		stop("# of colors must be equal to # of factor levels")
	lb = (1:n - 0.5)/max(n, stre) # place of the labels
	poly <- vector(mode="list", length(col))
	breaks = (0:n) / max(n, stre)
	if (n < stre) { # center
		breaks = breaks + (stre - n)/(2 * stre)
		lb = lb + (stre - n)/(2 * stre)
	}
	for (i in seq(poly))
		poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	for(i in seq(poly)) {
		if (axis.pos %in% c(1,3))
			polygon(poly[[i]], c(1,1,1-frc,1-frc), col=col[i], border=NA)
		if (axis.pos %in% c(2,4))
			polygon(c(0,0,frc,frc), poly[[i]], col=col[i], border=NA)
	}
	b = c(breaks[1], breaks[length(breaks)])
	if (axis.pos %in% c(1,3)) {
		lines(y = c(1,1-frc,1-frc,1,1), x = c(b[1],b[1],b[2],b[2],b[1]))
		# text(x = 1.05 * frc, y = lb, levels(z), pos = axis.pos)
		text(y = (1-frc)/1.05, x = lb, levels(z), pos = axis.pos)
	}
	if (axis.pos %in% c(2,4)) {
		lines(x = c(0,frc,frc,0,0), y = c(b[1],b[1],b[2],b[2],b[1]))
		text(x = 1.05 * frc, y = lb, levels(z), pos = axis.pos)
	}
}

plot.SpatialGridDataFrame = function(x, ..., attr = 1, col, breaks,
		zlim = range(as.numeric(x[[attr]])[is.finite(x[[attr]])]),
		axes = FALSE, xaxs = "i", yaxs = xaxs, at = NULL,
		border = NA, axis.pos = 4, add.axis = TRUE, what = "both", scale.size = lcm(2.8),
		scale.shrink = 0, scale.frac = 0.3, scale.n = 15) {

	if (missing(col)) {
		if (is.factor(x[[1]]))
			col = RColorBrewer::brewer.pal(nlevels(x[[1]]), "Set2")
		else
			col = bpy.colors(100) # heat.colors(12)
	}
	image.args = list(x = x, col = col, zlim = zlim, axes = axes, xaxs = xaxs, yaxs = yaxs, ...)
	if (all(c("red", "green", "blue") %in% names(image.args))) # legend off:
		what = "image"	
	else # plot band 1:
		image.args$x = x[1]
	if (!missing(breaks))
		image.args$breaks = breaks
	si = scale.size
	if (what == "both")
		switch (axis.pos,
			layout(matrix(c(2,1), nrow=2, ncol=1), widths=1, heights=c(1,si)),  # 1
			layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(si,1), heights=1),  # 2
			layout(matrix(c(1,2), nrow=2, ncol=1), widths=1, heights=c(si,1)),  # 3
			layout(matrix(c(2,1), nrow=1, ncol=2), widths=c(1,si), heights=1)   # 4
		)
	# scale:
	if (what %in% c("both", "scale")) {
		mar = c(1,1,1,1)
		if (! is.factor(x[[1]]))
			mar[axis.pos] = 3
		if (axes && axis.pos %in% c(2,4))
			mar[1] = 3
		if (axes && axis.pos %in% c(1,3))
			mar[2] = 3
		par(mar = mar)
		if (is.factor(x[[1]]))
			image.scale.factor(x[[1]], col = col, axis.pos = axis.pos, scale.frac = scale.frac, 
				scale.n = scale.n)
		else
			image.scale(x[[1]], zlim = zlim, col = col, breaks = breaks, axis.pos = axis.pos,
				add.axis = add.axis, at = at, shrink = scale.shrink)
		# axis(axis.pos)
	}
	if (what %in% c("both", "image")) {
		if (is.factor(x[[1]]))
			image.args$x[[1]] = as.numeric(x[[1]])
		mar=c(1,1,1,1)
		if (axes)
			mar[1:2] = 3
		par(mar = mar)
		do.call(image, image.args)
		if (!is.na(border))
			plot(geometry(x), col = border, add = TRUE)
	}
}

setMethod("plot", signature(x = "SpatialGridDataFrame", y = "missing"), 
	function(x,y,...) plot.SpatialGridDataFrame(x,...))

setMethod("plot", signature(x = "SpatialPixelsDataFrame", y = "missing"), 
	function(x,y,...) plot.SpatialGridDataFrame(x,...))
