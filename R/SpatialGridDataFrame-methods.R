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
	as(x, "matrix", ...)
}

as.array.SpatialGridDataFrame = function(x,...) {
    d = gridparameters(x)$cells.dim
    if (ncol(x@data) > 1)
        d = c(d, ncol(x@data))
    array(do.call(c, x@data), dim = d)
}
setAs("SpatialGridDataFrame", "array", function(from) 
	as.array.SpatialGridDataFrame(from))

as.matrix.SpatialGridDataFrame = function(x, ..., byrow = FALSE) {
	if (ncol(x@data) > 1)
		warning(
		"as.matrix.SpatialPixelsDataFrame uses first column;\n pass subset or [] for other columns")
	# try, at some stage also:
	# matrix(x@data[[1]], x@grid@cells.dim[2], x@grid@cells.dim[1], byrow=TRUE)
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
