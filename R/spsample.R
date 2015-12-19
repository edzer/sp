makegrid = function(x, n = 10000, nsig = 2, cellsize, 
		offset = rep(0.5, nrow(bb)), pretty = TRUE) {
	if (is(x, "Spatial"))
		bb = bbox(x)
	else
		bb = x
	if (missing(cellsize)) {
		pw = 1.0/nrow(bb)
		cellsize = signif((prod(apply(bb, 1, diff))/n) ^ pw, nsig)
	}
	if (length(cellsize) == 1)
		cellsize = rep(cellsize, nrow(bb))
	# find pretty grid coordinates:
	min.coords = bb[,1] + offset * cellsize
	if (pretty)
		min.coords = signif(min.coords, max(ceiling(log10(abs(bb[1,]) / cellsize))))
	sel = min.coords - offset * cellsize > bb[,1]
	if (any(sel))
		min.coords[sel] = min.coords[sel] - cellsize[sel]
	expand.grid.arglist = list()
	for (i in 1:nrow(bb)) {
		name = paste("x", i, sep = "")
		from = min.coords[i]
		by = cellsize[i]
		length.out = round(1 + (bb[i,2] - from) / by)
		expand.grid.arglist[[name]] = seq(from, by=by, length.out=length.out)
	}
	xy = do.call(expand.grid, expand.grid.arglist)
	attr(xy, "cellsize") = cellsize
	return(xy)
}

makegrid0 = function(x, n = 10000, nsig = 2, cellsize, 
		offset = rep(0.5,nrow(bb))) {
#cat("n in makegrid", n, "\n")
	if (is(x, "Spatial"))
		bb = bbox(x)
	else
		bb = x
	# rx = bb[1,]
	# ry = bb[2,]
	if (missing(cellsize)) {
		pw = 1.0/nrow(bb)
		cellsize = signif((prod(apply(bb, 1, diff))/n) ^ pw, nsig)
		cellsize = min(cellsize, min(apply(bb, 1, diff)))
	}
	if (length(cellsize) == 1)
		cellsize = rep(cellsize, nrow(bb))
# in some cases with small n, min* can be larger than bbox max values
# so guard imposed to step down from cellsize
	min.coords = pmax(bb[,1], signif(bb[,1] + offset * cellsize, nsig))
	# minx = max(rx[1], signif(rx[1] + offset[1] * cellsize, nsig))
	# miny = max(ry[1], signif(ry[1] + offset[2] * cellsize, nsig))
	expand.grid.arglist = list()
	for (i in 1:nrow(bb)) {
		name = paste("x", i, sep = "")
		sign = ifelse(min.coords[i] < bb[i,2], 1, -1)
		expand.grid.arglist[[name]] = seq(min.coords[i], bb[i,2], sign * cellsize[i])
	}
	# if (minx < rx[2]) seqx = seq(minx, rx[2], by = cellsize)
	# else seqx = seq(minx, rx[2], by = -cellsize)
	# if (miny < ry[2]) seqy = seq(miny, ry[2], by = cellsize)
	# else seqy = seq(miny, ry[2], by = -cellsize)
	# type = "regular" :
	xy = do.call(expand.grid, expand.grid.arglist)
	attr(xy, "cellsize") = cellsize
	return(xy)
}

sample.Spatial = function(x, n, type, bb = bbox(x), offset = runif(nrow(bb)), 
		cellsize, ..., nclusters = 1) {

	if (missing(n)) n <- as.integer(NA)
	n <- ceiling(n)
#cat("n in sample.Spatial", n, "\n")
	if (type == "random")
		xy = apply(bb, 1, function(x) runif(n) * diff(x) + x[1])
	else if (type == "Fibonacci") {
		if (!identical(is.projected(x), FALSE))
			warning("Fibonacci sampling is supposed to work on long/lat only")
		xy = fiboGrid(n %/% 2, bb[1,], bb[2,])
	} else if (type == "hexagonal")
		xy = hexGrid(bb, n = n, offset = offset, cellsize = cellsize)
	else {
		if (is.na(n))
			xy = makegrid0(bb, nsig = 20, cellsize = cellsize, 
				offset = offset)
		else
			xy = makegrid0(bb, n = n, nsig = 20, cellsize = cellsize,
				offset = offset)
		cellsize = attr(xy, "cellsize")
		if (type == "stratified") {
			n = nrow(xy)
			for (j in 1:ncol(xy))
				xy[,j] = xy[,j] + (runif(n) - 0.5) * cellsize[j]
		} else if (type == "clustered") {
			n = nrow(xy) # BG, Mon May  9 14:58:18 CEST 2011
			clus = rep(sample(1:n, nclusters, replace = FALSE), length = n)
			for (j in 1:ncol(xy))
				xy[,j] = xy[clus,j] + (runif(n) - 0.5) * cellsize[j]
		} else if (type == "nonaligned") {
			if (ncol(xy) != 2)
				stop("sorry, nonaligned is only implemented for 2D")
			nx = length(unique(xy[,1]))
			ny = length(unique(xy[,2]))
			x0 <- rep(runif(ny), rep(nx, ny))
			y0 <- rep(runif(nx), ny)
			xy[,1] = xy[,1] + (x0 - 0.5) * cellsize[1]
			xy[,2] = xy[,2] + (y0 - 0.5) * cellsize[2]
		} else if (type != "regular")
			stop(paste("sampling type", type, "not recognized"))
	}
# Patrick Girardoux 080217
	if (!is.na(n) && n == 1 && !is.matrix(xy) && is.vector(xy)) 
		xy <- matrix(xy, ncol=nrow(bb))
	sel = xy[,1] >= bb[1,1] & xy[,1] <= bb[1,2] & 
			xy[,2] >= bb[2,1] & xy[,2] <= bb[2,2]
	xy = xy[sel,, drop = FALSE]
	rownames(xy) = NULL
	SpatialPoints(xy, CRS(proj4string(x)))
}

setMethod("spsample", signature(x = "Spatial"), sample.Spatial)

sample.Line = function(x, n, type, offset = runif(1), proj4string = CRS(as.character(NA)), ...) {
	offset = offset[1]
	if (missing(n)) n <- as.integer(NA)
	if (!is.finite(n) || n < 1) return(NULL)
	cc = coordinates(x)
        lengths = LineLength(cc, longlat=FALSE, sum=FALSE)
        if (any(abs(lengths) < .Machine$double.eps)) {
	    wl <- which(abs(lengths) < .Machine$double.eps)
            cc <- cc[-(wl),]
            lengths <- lengths[-(wl)]
        }
	csl = c(0, cumsum(lengths))
	maxl = csl[length(csl)]
	if (type == "random")
		pts = runif(n) * maxl 
	else if (type == "stratified")
		pts = ((1:n) - runif(n))/n * maxl
	else if (type == "regular")
		pts = ((1:n) - (1-offset))/n * maxl
	else
		stop(paste("type", type, "not available for Line"))
	# find coordinates:
	int = findInterval(pts, csl, all.inside = TRUE)
	where = (pts - csl[int])/diff(csl)[int]
	xy = cc[int, , drop=FALSE] + where * (cc[int+1, , drop=FALSE] - 
	    cc[int, , drop=FALSE])
	if (nrow(xy) < 1) return(NULL)
	SpatialPoints(xy, proj4string)
}
setMethod("spsample", signature(x = "Line"), sample.Line)

sample.Lines = function(x, n, type, offset = runif(1), ...) {
	L = x@Lines
	lengths = sapply(L, function(x) LineLength(x@coords))
        if (sum(lengths) < .Machine$double.eps)
	    stop("Lines object of no length")
	nrs = round(lengths / sum(lengths) * n)
	ret = vector("list", sum(nrs > 0))
	j = 1
	for (i in 1:length(L)) {
		if (nrs[i] > 0) {
			ret[[j]] = sample.Line(L[[i]], nrs[i], type = type, offset = offset, ...)
			j = j+1
		}
	}
	do.call(rbind, ret)
}
setMethod("spsample", signature(x = "Lines"), sample.Lines)

sample.SpatialLines = function(x, n, type, offset = runif(1), ...) {
	# lengths = SpatialLinesLengths(x, longlat = isTRUE(!is.projected(x)))
	if (isTRUE(!is.projected(x)))
		warning("working under the assumption of projected data!")
	lengths = SpatialLinesLengths(x, longlat = FALSE)
	if (sum(lengths) < .Machine$double.eps)
		stop("SpatialLines object of no length")
	nrs = round(lengths / sum(lengths) * n)
	if (sum(nrs) == 0) 
		warning("n too small, increase n and sample from output")
	ret = vector("list", sum(nrs > 0))
	j = 1
	for (i in 1:length(lengths)) {
		if (nrs[i] > 0) {
			ret[[j]] = sample.Lines(x@lines[[i]], nrs[i], type = type, 
				offset = offset, ...)
			j = j+1
		}
	}
	ret = do.call(rbind, ret)
	if (!is.null(ret)) 
		proj4string(ret) = CRS(proj4string(x))
	ret
}
setMethod("spsample", signature(x = "SpatialLines"), sample.SpatialLines)

sample.Polygon = function(x, n, type = "random", bb = bbox(x),
	offset = runif(2), proj4string=CRS(as.character(NA)), iter=4, ...) {
	if (missing(n)) n <- as.integer(NA)
#cat("n in sample.Polygon", n, "\n")
	area = slot(x, "area")
	if (area == 0.0)
		spsample(Line(slot(x, "coords")), 
			n, type, offset = offset[1], proj4string=proj4string)
#CRS(proj4string(x))), n, type, offset = offset[1])
	else {
		res <- NULL
		its <- 0
		n_now <- 0
		bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	        bb.area <- bb.area + bb.area*its*0.1
		xSP <- new("Spatial", bbox=bbox(x), proj4string=proj4string)
		if (type == "random") {
		    brks <- c(1,3,6,10,20,100)
		    reps <- c(5,4,3,2,1.5)
		    n_is <- round(n * reps[findInterval(n,
			brks, all.inside=TRUE)] * bb.area/area)
		} else n_is <- round(n * bb.area/area)
		while (is.null(res) && its < iter && n_is > 0 && 
		   	ifelse(type == "random", (n_now < n), TRUE)) {
		    pts = sample.Spatial(xSP, n_is, type=type, 
			offset = offset, ...)
		    id = over(pts, SpatialPolygons(list(Polygons(list(x),
				"xx")), proj4string=proj4string))
		    Not_NAs <- !is.na(id)
		    if (!any(Not_NAs)) res <- NULL
		    else res <- pts[which(Not_NAs)]
		    if (!is.null(res)) n_now <- nrow(res@coords)
		    its <- its+1
		}
		if (type == "random")
		    if (!is.null(res) && n < nrow(res@coords)) 
			res <- res[sample(nrow(res@coords), n)]
		res
	}
}
setMethod("spsample", signature(x = "Polygon"), sample.Polygon)

sample.Polygons = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), proj4string=CRS(as.character(NA)), iter=4, ...) {
	if (missing(n)) n <- as.integer(NA)
	area = sapply(slot(x, "Polygons"), function(i) slot(i, "area")) # also available for Polygons!
	if (sum(area) == 0.0)
		# distribute n over the lines, according to their length?
		stop("sampling over multiple lines not functioning yet...")
	res <- NULL
	its <- 0
	holes <- sapply(slot(x, "Polygons"), function(y) slot(y, "hole"))
	pls <- slot(x, "Polygons")
	smple <- rep(TRUE, length(pls))
	if (length(pls) > 1) {
#	    for (i in seq(along=pls)) {
#		bbi <- .bbox2SPts(bbox(pls[[i]]), proj4string=proj4string)
#		bb_in <- lapply(pls[-i], function(x, pts) 
#			pointsInPolygon(pts, x), pts = bbi)
# added condition 100716
#                zzz <- do.call("rbind", bb_in)
#		if (holes[i] || (any(unlist(bb_in) == 1) &&
#                    !(sum(zzz[i,]) %% 2) == 0)) smple[i] <- FALSE
# was [,i], changed to [i,] 110307 Aman Verma
#	    }
            smple <- !holes
# all heuristics removed, hole slots must be correctly defined 110308
	}
	sum_area <- sum(area[smple])
	while (is.null(res) && its < iter) {
	    ptsres <- vector(mode="list", length=length(area))
	    for (i in seq(along=ptsres)) {
		if (smple[i]) {
		    nnow <- ceiling(n*(area[i]/sum_area))
		    ptsres[[i]] <- sample.Polygon(x=pls[[i]], 
		    n=nnow, type = type, offset = offset, iter=iter)
		}
	    }
	    crds <- do.call(rbind, lapply(ptsres, function(x) 
	        if (!is.null(x)) {rownames(x) <- NULL; coordinates(x)}))
# RSB 151013 oddity - duplicate rownames
	    if (is.null(crds)) res <- NULL
	    else {
	        pts <- SpatialPoints(crds, proj4string=proj4string)
	        id = over(pts, SpatialPolygons(list(x), proj4string=proj4string))
	        Not_NAs <- !is.na(id)
	        if (!any(Not_NAs)) res <- NULL
	        else res <- pts[which(Not_NAs)]
# Patrick Girardoux 080217
#	        if (type == "random" && nrow(res@coords) < n) res <- NULL
	        if(!is.null(res))
	            if (type == "random" && nrow(res@coords) < n) res <- NULL
	    }
	    its <- its+1
	}
	if (type == "random")
	    if (!is.null(res) && n < nrow(res@coords)) 
		res <- res[sample(nrow(res@coords), n)]
	res
}
setMethod("spsample", signature(x = "Polygons"), sample.Polygons)

sample.SpatialPolygons = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), iter=4, ...) {
	#stop("not functioning yet...")
	if (missing(n)) n <- as.integer(NA)
#cat("n in sample.SpatialPolygons", n, "\n")
	# EJP, 12/6/07: replaced area calculation with negative areas for holes...
	#area = sum(unlist(lapply(slot(x, "polygons"), function(x) slot(x, "area"))))
	getArea = function(x) {
    		getAreaPolygons = function(x) {
        		holes = unlist(lapply(x@Polygons, function(x) x@hole))
        		areas = unlist(lapply(x@Polygons, function(x) x@area))
        		area = ifelse(holes, -1, 1) * areas
        		area
    		}
    		sum(unlist(lapply(x@polygons, getAreaPolygons)))
	}
	area = getArea(x)
	if (area <= 0.0)
		stop("cannot sample in zero-area polygons")
	res <- NULL
	its <- 0
	bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	n_tot = round(n * bb.area/area) 
	while (is.null(res) && its < iter) {
	    # enlarge n each iteration:
	    pts = sample.Spatial(as(x, "Spatial"), n_tot * (1 + its * 0.1), 
	    	type=type, offset = offset, ...)
	    Over_pts_x <- over(pts, geometry(x))
	    Not_NAs <- !is.na(Over_pts_x)
	    if (!any(Not_NAs)) res <- NULL
	    else res <- pts[Not_NAs]
# Patrick Girardoux 080217
#	    if (type == "random" && nrow(res@coords) < n) res <- NULL
	    if(!is.null(res))
	         if (type == "random" && nrow(res@coords) < n) res <- NULL
	    its <- its+1
	}
	if (type == "random")
	    if (!is.null(res) && n < nrow(res@coords)) 
		res <- res[sample(nrow(res@coords), n)]
	if (is.null(res))
		stop("iteration did not converge; try enlarging argument iter")
	proj4string(res) = CRS(proj4string(x))
	res
}
setMethod("spsample", signature(x = "SpatialPolygons"), sample.SpatialPolygons)

sample.Sgrid = function(x, n, type = "random", bb = bbox(x),
		offset = runif(nrow(bb)), ...) {
	if (missing(n)) n <- as.integer(NA)
#cat("n in sample.Sgrid", n, "\n")
	area = areaSpatialGrid(x)
	if (area == 0.0)
		stop("cannot sample from grid with zero area")
	pts = spsample(as(x, "Spatial"), n, type, offset = offset, ...)
	pts[!is.na(over(pts, geometry(x)))]
}
setMethod("spsample", signature(x = "SpatialGrid"), sample.Sgrid)

sample.Spixels = function(x, n, type = "random", bb = bbox(x),
		offset = runif(nrow(bb)), ...) {
	if (missing(n)) n <- as.integer(NA)
#cat("n in sample.Spixels", n, "\n")
	area = areaSpatialGrid(x)
	if (area == 0.0)
		stop("cannot sample from grid with zero area")
	bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	pts = spsample(as(x, "Spatial"), round(n * bb.area/area), type, 
		offset = offset, ...)
	pts[!is.na(over(pts, geometry(x)))]
}
setMethod("spsample", signature(x = "SpatialPixels"), sample.Spixels)

hexGrid = function(bb, n, offset, cellsize) {
	if (missing(cellsize)) {
		if (missing(n))
			stop("need either cellsize or n")
		area = prod(apply(bb, 1, diff))/n
#		dx = area / (sqrt(3)/2)
#		suggested by Don MacQueen, macq@llnl.gov, Fri Mar 14 16:00:07 CET 2008
		dx = sqrt(area)/(sqrt(3)/2)
	} else
		dx = cellsize
	xy = genHexGrid(dx, bb[,1], bb[,2])

#		also suggested by Don MacQueen, macq@llnl.gov, Fri Mar 14 16:00:07 CET 2008:
	xy[,1] <- xy[,1] + offset[1] * dx 
	xy[,2] <- xy[,2] + offset[2] * dx * sqrt(3)/2

	attr(xy, "cellsize") = dx
	xy
}

# THK, posted to r-sig-geo, 03/03/2007:

genHexGrid <- function(dx, ll = c(0, 0), ur = c(1, 1)) {
        dy <- sqrt(3) * dx / 2

        x <- seq(ll[1], ur[1] - dx / 2, dx)
        y <- seq(ll[2], ur[2], dy)

        y <- rep(y, each = length(x))
        x <- rep(c(x, x + dx / 2), length.out = length(y))

        x <- x + (ur[1] - max(x)) / 2
        y <- y + (ur[2] - max(y)) / 2

        data.frame(x = x, y = y)
}

genPolyList <- function(hexGrid, dx) {
	# EJP; changed:
	# how to figure out dx from a grid? THK suggested:
        #dx <- hexGrid$x[2] - hexGrid$x[1]
	# and the following will also not allways work:
	if (missing(dx))
		dx = 2 * min(diff(sort(unique(hexGrid$x))))
	dy <- dx / sqrt(3)

	x.offset <- c(-dx / 2, 0, dx / 2, dx / 2, 0, -dx / 2, -dx / 2)
	y.offset <- c(dy / 2, dy, dy / 2, -dy / 2, -dy, -dy / 2, dy / 2)

	f <- function(i) list(x = hexGrid$x[i] + x.offset,
		y = hexGrid$y[i] + y.offset)

	ret = lapply(1:length(hexGrid$x), f)
}

# EJP, added:
HexPoints2SpatialPolygons = function(hex, dx) {
	ret = genPolyList(data.frame(coordinates(hex)), dx = dx)
	npoly = length(ret)
	Srl <- vector(mode="list", length=npoly)
	IDS = paste("ID", 1:npoly, sep="")
	for (i in 1:npoly)
		Srl[[i]] = Polygons(list(Polygon(ret[[i]])), IDS[i])
	res <- SpatialPolygons(Srl, proj4string=CRS(proj4string(hex)))
	res
}

fiboGrid <- function(N, xlim = c(-180,180), ylim = c(-90,90)) {
	if (max(xlim) <= 180)
		subtr = 180
	else
		subtr = 0
    phi = (1 + sqrt(5))/2
    i = seq(-N, N)
    P = 2 * N + 1
    lat = asin(2*i / P) * 180 / pi
    lon = ((2 * pi * i / phi) %% pi) * 360 / pi - subtr
    sel = lon <= xlim[2] & lon >= xlim[1] & lat <= ylim[2] & lat >= ylim[1]
    cbind(lon, lat)[sel, ]
}
