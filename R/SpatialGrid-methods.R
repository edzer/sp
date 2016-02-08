SpatialPixels = function(points, tolerance = sqrt(.Machine$double.eps), 
		proj4string = CRS(as.character(NA)), round = NULL, grid = NULL) {
	if (!is(points, "SpatialPoints"))
		stop("points should be of class or extending SpatialPoints")
	is.gridded = gridded(points)
	if (is.na(proj4string(points))) 
		proj4string(points) = proj4string
	if (is.null(grid))
		grid = points2grid(points, tolerance, round)
	if (!is.gridded) {
		points@bbox[,1] = points@bbox[,1] - 0.5 * grid@cellsize
		points@bbox[,2] = points@bbox[,2] + 0.5 * grid@cellsize
	}
	new("SpatialPixels", points, grid = grid, 
		grid.index = getGridIndex(coordinates(points), grid))
}

SpatialGrid = function(grid, proj4string = CRS(as.character(NA))) {
	stopifnot(is(grid, "GridTopology"))
	if (is.character(proj4string))
		proj4string = CRS(proj4string)
	cc = rbind(grid@cellcentre.offset - 0.5 * grid@cellsize,
			grid@cellcentre.offset + (grid@cells.dim - 0.5) * grid@cellsize)
	bb = .bboxCoords(cc)
	new("SpatialGrid", grid = grid, bbox = bb, proj4string = proj4string)
}

setAs("SpatialGrid", "SpatialPixels", 
	function(from) { 
		pts = as(from, "SpatialPoints")
		new("SpatialPixels", pts, grid = from@grid, grid.index = 1:length(pts))
	}
)

setAs("SpatialGrid", "SpatialPoints", 
	function(from) SpatialPoints(coordinates(from), from@proj4string))

setMethod("coordinates", "SpatialPixels", function(obj) obj@coords)

row.names.SpatialPixels <- function(x) {
    ret = dimnames(slot(x, "coords"))[[1]]
	if (is.null(ret))
		ret = seq_len(nrow(x@coords))
	ret
}

row.names.SpatialGrid <- function(x) 1:prod(x@grid@cells.dim)

setMethod("coordinates", "SpatialGrid", function(obj) coordinates(obj@grid))

setMethod("plot", signature(x = "SpatialGrid", y = "missing"),
    function(x, y, ..., grid = TRUE) {
		if (grid)
			plot.SpatialGrid(x, ...)
		else
			plot(as(x, "SpatialPoints"), ...)
	}
)

setMethod("plot", signature(x = "SpatialPixels", y = "missing"),
    function(x, y, ..., grid = TRUE) {
		if (grid)
			plot.SpatialPixels(x, ...)
		else
			plot(as(x, "SpatialPoints"), ...)
	}
)

coordnamesSG = function(x, value) {
	dimnames(x@bbox)[[1]] = value
	if (is(x, "SpatialPixels"))
		dimnames(x@coords)[[2]] = value
	coordnames(x@grid) = value
	x
}

setReplaceMethod("coordnames", 
	signature(x = "SpatialGrid", value = "character"), coordnamesSG)
setReplaceMethod("coordnames", 
	signature(x = "SpatialPixels", value = "character"), coordnamesSG)

getGridTopology = function(obj) {
	if (!(is(obj, "SpatialPixels") || is(obj, "SpatialGrid")))
		stop("object is not or does not extend class SpatialPixels or SpatialGrid")
	obj@grid
}

areaSpatialGrid = function(obj) {
	cellarea = prod(obj@grid@cellsize)
	if (is(obj, "SpatialGrid"))
		return(prod(obj@grid@cells.dim) * cellarea)
	else # take number of cells:
		length(obj@grid.index) * cellarea
}

gridparameters = function(obj) { 
	if (is(obj, "SpatialPixels") || is(obj, "SpatialGrid"))
		obj = obj@grid
	if (is(obj, "GridTopology"))
		return(data.frame(
			cellcentre.offset = obj@cellcentre.offset,
			cellsize = obj@cellsize,
			cells.dim = obj@cells.dim))
	return(numeric(0))
}

getGridIndex = function(cc, grid, all.inside = TRUE) {
	n = ncol(cc)
	idx = rep(1, nrow(cc))
	cumprod = 1
	for (i in 1:n) {
		this.idx = round((cc[,i] - grid@cellcentre.offset[i])/grid@cellsize[i])
		if (i == 2)
			this.idx = grid@cells.dim[2] - (this.idx + 1)
		outside = this.idx >= grid@cells.dim[i] | this.idx < 0
		if (any(outside)) {
			if (all.inside) {
				print(summary(this.idx))
				stop("this.idx out of range")
			} else
				this.idx[outside] = NA
		}
		idx = idx + this.idx * cumprod
		cumprod = cumprod * grid@cells.dim[i]
	}
	outside = idx < 1 | idx > .NumberOfCells(grid)
	if (any(na.omit(outside))) {
		print(summary(idx))
		stop("index outside boundaries")
	}
	as.integer(round(idx))
}

rcFromGridIndex = function(obj) { # returns <x,y> entries: col,row rather than row,col
	obj = as(obj, "SpatialPixels")
	gi = obj@grid.index
	grid = obj@grid
	stopifnot(ncol(coordinates(obj)) == 2)
	xi = ((gi - 1) %% grid@cells.dim[1]) + 1
	yi = grid@cells.dim[2] - ((gi - 1) %/% grid@cells.dim[1])
	cbind(xi,yi)
}

gridIndex2nb = function(obj, maxdist = sqrt(2), fullMat = TRUE, ...) {
	xy = rcFromGridIndex(obj)
	if (fullMat)
		lst = apply(as.matrix(dist(xy, ...)), 1, function(x) which(x <= maxdist))
	else {
		dst = function(X, Xi) apply(X, 1, function(Y) sqrt(sum((Y-Xi)^2)))
		n = nrow(xy)
		lst = vector(mode = "list", length = n)
		for (i in 1:n) { # avoid the n x n matrix construction:
			d = dst(xy, xy[i,])
			lst[[i]] = which(d <= maxdist)
		}
	}
	class(lst) = c("nb", "list")
	lst
}

setMethod("[", "SpatialPixels",
	function(x, i, j, ..., drop = FALSE) {
		if (!missing(j))
			stop("can only select pixels with a single index")
		if (missing(i))
			return(x)
		if (is(i, "Spatial"))
			i = !is.na(over(x, geometry(i)))
		if (drop) { # if FALSE: adjust bbox and grid
			res = as(x, "SpatialPoints")[i]
			tolerance = list(...)$tolerance
			if (!is.null(tolerance))
				res = SpatialPixels(res, tolerance = tolerance)
			else
				gridded(res) = TRUE
			res
		} else # default: don't adjust bbox and grid
			new("SpatialPixels", bbox = x@bbox, 
				proj4string = x@proj4string,	
				coords = x@coords[i, , drop = FALSE], grid = x@grid, 
				grid.index = x@grid.index[i])
	}
)

setMethod("[", "SpatialGrid",
	function(x, i, j, ..., drop = TRUE) {
		drop <- FALSE
#		if (!missing(drop))
#			stop("don't supply drop: it needs to be FALSE anyway")
		gr = x@grid
		if (missing(i))
			rows = 1:gr@cells.dim[2]
		else {
			if (is(i, "Spatial"))
				stop("area selection only makes sense for objects of class SpatialPixels or SpatialGridDataFrame; for object of class SpatialGrid you can only select x[rows,cols]")
			rows = i
		}
		if (missing(j))
			cols = 1:gr@cells.dim[1]
		else
			cols = j
		idx = 1:prod(gr@cells.dim[1:2])
		if (any(is.na(rows)) || any(is.na(cols))) 
			stop("NAs not permitted in indices")
		m = matrix(idx, gr@cells.dim[2], gr@cells.dim[1], byrow = TRUE)[rows,cols]
		idx = as.vector(m) # t(m)?
		cc = SpatialPixels(SpatialPoints(coordinates(x)[idx,,drop=FALSE], CRS(proj4string(x))))
		cc = as(cc, "SpatialGrid")
		cc
	}
)

setAs("SpatialPixels", "SpatialGrid", function(from) SpatialGrid(from@grid, from@proj4string))
#setAs("SpatialGrid", "SpatialPixels", 
#	function(from) {
#		pts = new("SpatialPoints", coords = coordinates(from),
#			bbox = from@bbox, proj4string = from@proj4string)
#		new("SpatialPixels", pts, grid = from@grid, grid.index = 1:NROW(cc))
#})

as.data.frame.SpatialPixels = function(x, row.names, optional, ...)
	as.data.frame(coordinates(x))

as.data.frame.SpatialGrid = as.data.frame.SpatialPixels

setAs("SpatialPixels", "data.frame", 
	function(from) as.data.frame.SpatialPixels(from))
setAs("SpatialGrid", "data.frame", 
	function(from) as.data.frame.SpatialGrid(from))

print.SpatialPixels = function(x, ...) {
	cat("Object of class SpatialPixels\n")
	print(summary(x@grid))
	cat("SpatialPoints:\n")
	print(coordinates(x))
	pst <- paste(strwrap(paste(
		"Coordinate Reference System (CRS) arguments:", 
		proj4string(x))), collapse="\n")
	cat(pst, "\n")
	invisible(x)
}
setMethod("show", "SpatialPixels", function(object) print.SpatialPixels(object))

print.SpatialGrid = function(x, ...) {
	cat("Object of class SpatialGrid\n")
	print(summary(x@grid))
	cat("SpatialPoints:\n")
	print(coordinates(x))
	pst <- paste(strwrap(paste(
		"Coordinate Reference System (CRS) arguments:", 
		proj4string(x))), collapse="\n")
	cat(pst, "\n")
	invisible(x)
}
setMethod("show", "SpatialGrid", function(object) print.SpatialGrid(object))

# make a SpatialPolygons from a SpatialPixels - Kohris Sahlen workshop
as.SpatialPolygons.SpatialPixels <- function(obj) {
	obj_crds <- coordinates(obj)
	IDs <- IDvaluesSpatialPixels(obj)
	nPolygons <- nrow(obj_crds)
	cS <- slot(slot(obj, "grid"), "cellsize")
	cS2 <- cS/2
	cS2x <- cS2[1]
	cS2y <- cS2[2]
	Srl <- vector(mode="list", length=nPolygons)
	for (i in 1:nPolygons) {
		xi <- obj_crds[i,1]
		yi <- obj_crds[i,2]
		x <- c(xi-cS2x, xi-cS2x, xi+cS2x, xi+cS2x, xi-cS2x)
		y <- c(yi-cS2y, yi+cS2y, yi+cS2y, yi-cS2y, yi-cS2y)
		Srl[[i]] <- Polygons(list(Polygon(coords=cbind(x, y))), ID=IDs[i])
                comment(Srl[[i]]) <- "0"
	}
	res <- SpatialPolygons(Srl, proj4string=CRS(proj4string(obj)))
	res
}
setAs("SpatialPixels", "SpatialPolygons", 
	function(from) as.SpatialPolygons.SpatialPixels(from))

IDvaluesSpatialPixels <- function(obj) {
	if (!is(obj, "SpatialPixels"))
		stop("function only works for objects of class or extending SpatialPixels")

	cc <- slot(obj, "grid.index")
	res <- as.matrix(sapply(cc, as.integer))
	paste("g", res, sep="")
}

length.SpatialPixels = function(x) { nrow(x@coords) }
length.SpatialGrid = function(x) { .NumberOfCells(x@grid) }

setAs("SpatialGrid", "SpatialPolygons", function(from) {
		ret = as.SpatialPolygons.GridTopology(from@grid)
		proj4string(ret) = proj4string(from)
		ret
	}
)
setMethod("coordnames", signature(x = "SpatialGrid"),
	function(x) dimnames(bbox(x))[[1]])

setReplaceMethod("coordnames", signature(x = "SpatialGrid", 
	value =  "character"),
    function(x, value) {
		dimnames(x@bbox)[[1]] = value
		coordnames(x@grid) = value
		x
	}
)

setAs("SpatialGrid", "GridTopology", function(from) getGridTopology(from))
setAs("SpatialPixels", "GridTopology", function(from) getGridTopology(from))

plot.SpatialGrid = function(obj, ..., col = par("fg"), 
		lty = par("lty"), lwd = par("lwd"), add = FALSE) {

  if (! add)
  	plot(as(obj, "Spatial"), ...)
  gr = obj@grid
  # Don MacQueen, Feb 12 2015
  csiz <- gr@cellsize
  ncells <- gr@cells.dim
  nbounds <- ncells+1
  
  ## first get and sort the cell centers
  cv <- coordinatevalues(gr)
  cv[[1]] <- sort(cv[[1]])
  cv[[2]] <- sort(cv[[2]])

  ## calculate cell boundaries
  cv[[1]] <- c(cv[[1]][1] - csiz[1]/2,  cv[[1]] + csiz[1]/2 )
  cv[[2]] <- c(cv[[2]][1] - csiz[2]/2,  cv[[2]] + csiz[2]/2 )
  
  ## construct endpoints of cell boundary lines
  ## vertical lines
  vfrom <- cbind(cv[[1]], cv[[2]][1])
  vto   <- cbind(cv[[1]], cv[[2]][nbounds[2]])

  ## horizontal lines
  hfrom <- cbind(cv[[1]][1], cv[[2]])
  hto   <- cbind(cv[[1]][nbounds[1]], cv[[2]])
    
  ## add to plot
  segments(vfrom[,1], vfrom[,2] , vto[,1], vto[,2], 
  	col = col, lty = lty, lwd = lwd)
  segments(hfrom[,1], hfrom[,2] , hto[,1], hto[,2],
  	col = col, lty = lty, lwd = lwd)
}

plot.SpatialPixels = function(obj, ..., col = par("fg"), 
		lty = par("lty"), lwd = par("lwd"), add = FALSE) {

	# based on plot.SpatialGrid:
	if (! add)
		plot(as(obj, "Spatial"), ...)

	gr = obj@grid
	csiz <- gr@cellsize
	ncells <- gr@cells.dim
	nbounds <- ncells + 1

	m = matrix(FALSE, ncells[2], ncells[1])
	rc = rcFromGridIndex(obj)[,c(2,1)]
	m[rc] = TRUE # the pattern
  
	## first get and sort the cell centers
	cv <- coordinatevalues(gr)
	cv[[1]] <- sort(cv[[1]])
	cv[[2]] <- sort(cv[[2]])

	## calculate cell boundaries
	x <- c(cv[[1]][1] - csiz[1]/2,  cv[[1]] + csiz[1]/2 )
	y <- c(cv[[2]][1] - csiz[2]/2,  cv[[2]] + csiz[2]/2 )
  
	# horizontal lines:
  	p = do.call(rbind, lapply(1:nbounds[2], function(i) {
			if (i == 1) # bottom line
				cells = m[1,]
			else if (i == nbounds[2]) # top line
				cells = m[nbounds[2]-1,]
			else # in-between, draw for:
				cells = m[i-1,] | m[i,]
			r = rle(cells) # figure out line pieces
			if (any(r$values)) {
				wr = which(r$values) # where to draw/end
				cs0 = c(0, cumsum(r$lengths)) # all start/end indices
				cbind(x[cs0[wr] + 1], y[i], x[cs0[wr+1] + 1], y[i])
			}
		}
	))
	segments(p[,1], p[,2], p[,3], p[,4], col = col, lty = lty, lwd = lwd)
	# vertical lines:
  	p = do.call(rbind, lapply(1:nbounds[1], function(i) {
			if (i == 1) # left boundary
				cells = m[,1]
			else if (i == nbounds[1]) # right boundary
				cells = m[,nbounds[1]-1]
			else # non-boundary lines, draw for:
				cells = m[,i-1] | m[,i]
			r = rle(cells)
			if (any(r$values)) {
				wr = which(r$values)
				cs0 = c(0, cumsum(r$lengths))
				cbind(x[i], y[cs0[wr]+1], x[i], y[cs0[wr+1]+1])
			}
		}
	))
	segments(p[,1], p[,2], p[,3], p[,4], col = col, lty = lty, lwd = lwd)
}
