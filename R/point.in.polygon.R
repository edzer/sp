point.in.polygon = function(point.x, point.y, pol.x, pol.y,
    mode.checked=FALSE) {
    if (mode.checked) res <- .Call(R_point_in_polygon_sp, point.x,
        point.y, pol.x, pol.y)
    else res <- .Call(R_point_in_polygon_sp, as.numeric(point.x),
        as.numeric(point.y), as.numeric(pol.x), as.numeric(pol.y))
    res
}

pointsInPolygon = function(pts, Polygon,
    mode.checked=FALSE) {
	pts = coordinates(pts)
	cc = slot(Polygon, "coords")
	point.in.polygon(pts[,1], pts[,2], cc[,1], cc[,2],
        mode.checked=mode.checked)
}

pointsInPolygons = function(pts, Polygons, #which = FALSE,
    mode.checked=FALSE) {
	rings = slot(Polygons, "Polygons")
	res = matrix(unlist(lapply(rings, function(x, pts) 
		pointsInPolygon(pts, x, mode.checked=mode.checked),
                pts = pts)), ncol=length(rings))
	res <- res > 0
#	holes <- sapply(rings, function(y) slot(y, "hole"))
#	areas <- sapply(rings, function(x) slot(x, "area"))
#	if (any(holes) && any(res[,holes])) {
#		holerows <- which(res[,holes,drop=FALSE], arr.ind=TRUE)[,1]
#		odd <- rowSums(res[holerows,,drop=FALSE])%%2 != 0
#		for (i in seq(along = holerows)) {
#			in_p <- which.min(areas[res[holerows[i],,drop=FALSE]])
#			res[holerows[i],] <- FALSE
#			if (odd[i]) res[holerows[i], in_p] <- TRUE
#		}
#		res[,holes] <- FALSE
#	}
# revised 100716
        ret <- rowSums(res) %% 2 != 0
#	ret <- apply(res, 1, any)
#	if (which) {
#		reta <- integer(length(ret))
#		for (i in seq(along = ret)) {
#			if (ret[i]) reta[i] <- which(res[i,])
#			else reta[i] <- as.integer(NA)
#		}
#		ret <- reta
#	}
	ret
}

pointsInSpatialPolygons0 = function(pts, SpPolygons) {
	sr = slot(SpPolygons, "polygons")
	res = lapply(sr, function(x, pts) pointsInPolygons(pts, x), pts = pts)
	#ret = rep(as.numeric(NA), nrow(coordinates(pts)))
	#for (i in seq(along = res))
	#	ret[res[[i]] > 0] = i
	#apply(do.call(rbind, res), 2, which)
	do.call(rbind, res)
}

pointsInSpatialPolygons = function(pts, SpPolygons, returnList = FALSE) {
    pls = slot(SpPolygons, "polygons")
    lb <- lapply(pls, function(x) as.double(bbox(x)))
    cpts <- coordinates(pts)
    storage.mode(cpts) <- "double"
    mode.checked <- storage.mode(cpts) == "double"
    cand <- .Call(tList, .Call(pointsInBox, lb, cpts[,1], cpts[,2]), 
		as.integer(length(pls)))
    # rm(cand0)
    # gc(verbose=FALSE)
    res <- pointsInPolys2(pls, cand, cpts, mode.checked=mode.checked,
		returnList = returnList)
    res
}

pointsInPolys2 <- function(pls, cand, pts, mode.checked=FALSE, 
		returnList = FALSE) {
    n <- nrow(pts)
    if (returnList)
		res = sapply(1:length(pls), function(x) integer(0))
	else
		res <- rep(as.integer(NA), n)
	# print(cand)
    for (i in seq(along=cand)) {
        candi <- cand[[i]]
        if (length(candi) > 0) {
            ptsi <- pts[candi,,drop=FALSE]
            ret <- pointsInPolygons(ptsi, pls[[i]], mode.checked=mode.checked)
            #for (j in seq(along=candi)) {
            #    jj <- candi[j]
            #    if (is.na(res[jj]))
			#		res[jj] <- ifelse(ret[j], i, as.integer(NA))
            #}
			if (returnList)
				res[[i]] = candi[ret]
			else
				res[candi[ret]] = i 
        }
    }
    res
}
