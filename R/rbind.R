makeUniqueIDs <- function(lst) {
	ids = sapply(lst, function(i) slot(i, "ID"))
	if (any(duplicated(ids))) {
		ids <- make.unique(as.character(unlist(ids)), sep = "")
		for (i in seq(along = ids))
			lst[[i]]@ID = ids[i]
	}
	lst
}

rbind.SpatialPoints <- function(...) {
	dots = list(...)
	names(dots) <- NULL
	stopifnot(identicalCRS(dots))
	dropRowNames = is.null(dimnames(dots[[1]]@coords)[[1]]) # or check each of them?
	coordinates.strip = function(x) {
		x = coordinates(x)
		row.names(x) = NULL
		x
	}
	ret = SpatialPoints(do.call(rbind, lapply(dots, coordinates.strip)),
		CRS(proj4string(dots[[1]])))
	if (!dropRowNames)
		row.names(ret) = make.unique(do.call(c, lapply(dots, row.names)))
	ret
}

rbind.SpatialPointsDataFrame <- function(...) {
	dots = list(...)
	names(dots) <- NULL # bugfix Clement Calenge 100417
	sp = do.call(rbind, lapply(dots, function(x) as(x, "SpatialPoints")))
	df = do.call(rbind, lapply(dots, function(x) x@data))
	SpatialPointsDataFrame(sp, df, coords.nrs = dots[[1]]@coords.nrs)
}


# contributed by Kent Johnson, r-sig-geo, Dec 5, 2015:
rbind.SpatialMultiPoints <- function(...) { 
	dots = list(...)
	names(dots) <- NULL
	stopifnot(identicalCRS(dots))
	SpatialMultiPoints(do.call(c, lapply(dots, slot, name="coords")),
	CRS(proj4string(dots[[1]])))
}

rbind.SpatialMultiPointsDataFrame <- function(...) {
	dots = list(...)
	names(dots) <- NULL
	sp = do.call(rbind, lapply(dots, function(x) as(x, "SpatialMultiPoints")))
	df = do.call(rbind, lapply(dots, function(x) x@data))
	SpatialMultiPointsDataFrame(sp, df)
}


rbind.SpatialPixels = function(...) {
	dots = list(...)
	names(dots) <- NULL
	sp = do.call(rbind, lapply(dots, function(x) as(x, "SpatialPoints")))
	gridded(sp) = T
	sp
}

rbind.SpatialPixelsDataFrame = function(...) {
	dots = list(...)
	names(dots) <- NULL
	sp = do.call(rbind, lapply(dots, function(x) as(x, "SpatialPointsDataFrame")))
	gridded(sp) = T
	sp
}

rbind.SpatialPolygons = function(..., makeUniqueIDs = FALSE) {
	dots = list(...)
	names(dots) <- NULL
	stopifnot(identicalCRS(dots))
	# checkIDSclash(dots)
	pl = do.call(c, lapply(dots, function(x) slot(x, "polygons")))
	if (makeUniqueIDs)
		pl = makeUniqueIDs(pl)
	SpatialPolygons(pl, proj4string = CRS(proj4string(dots[[1]])))
}

rbind.SpatialPolygonsDataFrame <- function(..., makeUniqueIDs = FALSE) {
	dots = list(...)
	names(dots) <- NULL # bugfix Clement Calenge 100417
	lst = lapply(dots, function(x) as(x, "SpatialPolygons"))
	lst$makeUniqueIDs = makeUniqueIDs
	pl = do.call(rbind.SpatialPolygons, lst)
	df = do.call(rbind, lapply(dots, function(x) x@data))
	SpatialPolygonsDataFrame(pl, df)
}


rbind.SpatialLines = function(..., makeUniqueIDs = FALSE) {
	dots = list(...)
	names(dots) <- NULL
	stopifnot(identicalCRS(dots))
	ll = do.call(c, lapply(dots, function(x) slot(x, "lines")))
	if (makeUniqueIDs)
		ll = makeUniqueIDs(ll)
	SpatialLines(ll, proj4string = CRS(proj4string(dots[[1]])))
}

rbind.SpatialLinesDataFrame <- function(...) {
	dots = list(...)
	names(dots) <- NULL # bugfix Clement Calenge 100417
	ll = do.call(rbind, lapply(dots, function(x) as(x, "SpatialLines")))
	df = do.call(rbind, lapply(dots, function(x) x@data))
	SpatialLinesDataFrame(ll, df)
}

cbind.Spatial <- function(...) {
	dots = list(...)
	names(dots) <- NULL
	stopifnot(identicalCRS(dots[ which(sapply(dots, function(x) is(x, "Spatial"))) ]))
	dfs = lapply(dots, function(x) if(is(x, "Spatial")) x@data else x)
	d = do.call(cbind, dfs)
	addAttrToGeom(geometry(dots[[1]]), data.frame(d), FALSE)
}
