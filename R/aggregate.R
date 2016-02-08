aggregate.data.frame.SP <- function (x, by, FUN, ..., dissolve = TRUE) {
	# taken from stats::aggregate.data.frame, in
	# R 3.1.0, Fri May 23 23:31:15 CEST 2014 svn rev 65387 
	# took out option simplify, as it doesn't make sense to not do that
    # moved "FUN <- match.fun(FUN)" to top caller

	# EP added:
	stopifnot(is(x, "Spatial"))
	stopifnot("data" %in% slotNames(x))
	geom = geometry(x)
	x = x@data

    if (NROW(x) == 0L) 
        stop("no rows to aggregate")
    if (NCOL(x) == 0L) {
        x <- data.frame(x = rep(1, NROW(x)))
        return(aggregate.data.frame(x, by, function(x) 0L)[seq_along(by)])
    }
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
    if (any(unlist(lapply(by, length)) != nrx)) 
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
    if (ncol(y)) 
        grp <- rank(do.call(paste, c(lapply(rev(y), ident), list(sep = "."))), 
            ties.method = "min")
    else grp <- integer(nrx)
    y <- y[match(sort(unique(grp)), grp, 0L), , drop = FALSE]
    nry <- NROW(y)
    z <- lapply(x, function(e) {
        ans <- lapply(X = split(e, grp), FUN = FUN, ...)
        if (length(len <- unique(sapply(ans, length))) == 1L) {
            if (len == 1L) {
                cl <- lapply(ans, oldClass)
                cl1 <- cl[[1L]]
                ans <- unlist(ans, recursive = FALSE)
                if (!is.null(cl1) && all(sapply(cl, function(x) identical(x, 
                  cl1)))) 
                  class(ans) <- cl1
            }
            else if (len > 1L) 
                ans <- matrix(unlist(ans, recursive = FALSE), 
                  nrow = nry, ncol = len, byrow = TRUE, dimnames = {
                    if (!is.null(nms <- names(ans[[1L]]))) 
                      list(NULL, nms)
                    else NULL
                  })
        }
        ans
    })
    len <- length(y)
    for (i in seq_along(z)) y[[len + i]] <- z[[i]]
    names(y) <- c(names(by), names(x))
    row.names(y) <- NULL

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
	if (identical(y$ID, rep(1, nrow(y))))
		y$ID = NULL # remove ID field
	addAttrToGeom(geom, y, match.ID = FALSE)
}

aggregate.Spatial = function(x, by = list(ID = rep(1, length(x))), FUN = mean, ..., 
		dissolve = TRUE, areaWeighted = FALSE) {
    FUN <- match.fun(FUN)
	if (is(by, "Spatial")) { # maybe better do S4 method dispatch?
		by0 = by
		if (gridded(by))
			by = as(by, "SpatialPolygons")
		if (is(x, "SpatialPolygonsDataFrame") && is(by, "SpatialPolygons") && areaWeighted)
			df = aggregatePolyWeighted(x, by)
		else
			df = over(by, x, fn = FUN, ...)
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
