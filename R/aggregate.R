aggregate.data.frame.SP <- function (x, by, FUN, ..., dissolve = TRUE) {

	# EP added:
	stopifnot(is(x, "Spatial"))
	stopifnot("data" %in% slotNames(x))
	geom = geometry(x)
	x = x@data

    stopifnot(NROW(x) > 0L) 
    stopifnot(NCOL(x) > 0L)
    FUN <- match.fun(FUN)

	# next: fragment taken from stats::aggregate.data.frame, in
	# R 3.2.4, svn 70336, to find grp
    if (!is.list(by)) 
        stop("'by' must be a list")
    if (is.null(names(by)) && length(by)) 
        names(by) <- paste("Group", seq_along(by), sep = ".")
    else {
        nam <- names(by)
        ind <- which(!nzchar(nam))
        names(by)[ind] <- paste("Group", ind, sep = ".")
    }
    nrx <- NROW(x)
    # if (any(lengths(by) != nrx)) 
	if (any(sapply(by, length) != nrx))
        stop("arguments must have same length")
    y <- as.data.frame(by, stringsAsFactors = FALSE)
    keep <- complete.cases(by)
    y <- y[keep, , drop = FALSE]
    x <- x[keep, , drop = FALSE]
    nrx <- NROW(x)
    ident <- function(x) {
        y <- as.integer(as.factor(x))
        z <- gsub(" ", "0", format(y, scientific = FALSE))
        return(z)
    }
    grp <- if (ncol(y)) {
        grp <- lapply(rev(y), ident)
        names(grp) <- NULL
        do.call(paste, c(grp, list(sep = ".")))
    } else integer(nrx)

	# let aggregate.data.frame do the attribute work:
	y = aggregate(x, by, FUN, ..., simplify = TRUE) 

	# original would now return y; I added:
	if (dissolve) { # dissolve/merge:
		if (!gridded(geom) && is(geom, "SpatialPoints"))
			geom = split(geom, factor(grp)) # creates SpatialMultiPoints
		else {
			if (!requireNamespace("rgeos", quietly = TRUE))
				stop("rgeos required")
			if (is(geom, "SpatialLines"))
				geom = rgeos::gLineMerge(geom, grp)
			else {
				if (gridded(geom))
					geom = as(geom, "SpatialPolygons")
				geom = rgeos::gUnaryUnion(geom, grp)
			}
		}
	} else
		y = y[as.integer(factor(grp)),,drop=FALSE] # repeat
	addAttrToGeom(geom, y, match.ID = FALSE)
}

aggregate.Spatial = function(x, by = list(ID = rep(1, length(x))), FUN, ..., 
		dissolve = TRUE, areaWeighted = FALSE) {
	if (is(by, "Spatial")) { # maybe better do S4 method dispatch?
		by0 = by
		if (gridded(by))
			by = as(by, "SpatialPolygons")
		if (is(x, "SpatialPolygonsDataFrame") && is(by, "SpatialPolygons") && areaWeighted) {
			if (!missing(FUN))
				warning("argument FUN is ignored in area-weighted aggregation, see documentation")
			df = aggregatePolyWeighted(x, by)
		} else {
    		FUN <- match.fun(FUN)
			df = over(by, x, fn = FUN, ...)
		}
		addAttrToGeom(by0, df, match.ID = FALSE)
	} else
		aggregate.data.frame.SP(x, by, FUN, ..., dissolve = dissolve)
}

aggregatePolyWeighted = function(x, by) {
	if (!requireNamespace("rgeos", quietly = TRUE))
		stop("rgeos required")
	i = rgeos::gIntersection(x, by, byid = TRUE, drop_lower_td = TRUE)
	area =  sapply(i@polygons, function(x) slot(x, name = "area"))
	ids.i = sapply(i@polygons, function(x) slot(x, name = "ID"))
	IDs = strsplit(ids.i, " ") # IDs, as list
	if (any(sapply(IDs, length) != 2)) # sanity check:
		stop("IDs contain spaces: this breaks identification after gIntersection()")
	grp = do.call(rbind, IDs) # IDs matrix; col 1 = x, col 2 = by
	obs = x[grp[, 1], ]@data # match by IDs of x: get the attributes to aggregate
	if (all(sapply(obs, is.factor))) { # find level with largest area ...
		obs$aReA = area
		spl = split(obs, grp[,2]) # grouped by `by'
		ret = do.call(rbind, lapply(spl, function(x) x[which.max(x$aReA),])) # take mode
		ret$aReA = NULL # clean up
		ret[match(row.names(by), row.names(ret)), , drop=FALSE] # match to by's order
	} else {
		if(any(sapply(obs, is.factor)))
			warning("for factor aggregation, provide factor only data")
		x_area = data.frame(lapply(obs, function(x) x * area))
		agg = aggregate(data.frame(area, x_area), list(grp[,2]), sum) 
		ret = data.frame(lapply(agg[-(1:2)], function(x) x / agg$area))
		ret[match(row.names(by), agg$Group.1), , drop=FALSE]
	}
}
