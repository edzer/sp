spDistsN1 <- function(pts, pt, longlat=FALSE) {
	if (inherits(pts, "SpatialPoints")) 
		pts <- coordinates(pts)
	if (!is.matrix(pts)) stop("pts must be a matrix")
	if (ncol(pts) != 2) stop("pts must have two columns")
	if (!is.numeric(pts)) stop("pts must be numeric")
	if (inherits(pt, "SpatialPoints")) 
		pt <- coordinates(pt)
	if (!is.numeric(pt)) stop("pt must be numeric")
	if (length(pt) != 2) stop("pt must have length two")
	storage.mode(pts) <- "double"
	storage.mode(pt) <- "double"
	x <- pts[,1]
	y <- pts[,2]
	xx <- pt[1]
	yy <- pt[2]
	n  <- as.integer(length(x))
	dists <- vector(mode="double", length=n)
	lonlat <- as.integer(longlat)
	res <- .C("sp_dists", x, y, xx, yy, n, dists, lonlat, 
		PACKAGE = "sp")[[6]]
	if (any(!is.finite(res))) {
		nAn <- which(!is.finite(res))
		dx <- abs(x[nAn] - xx)
		dy <- abs(y[nAn] - yy)
		if (all((c(dx, dy) < .Machine$double.eps ^ 0.5)))
			res[nAn] <- 0
		else
			stop(paste("non-finite distances in spDistsN1"))
	}
	res
}

spDists <- function(x, y = x, longlat = FALSE, segments = FALSE, diagonal = FALSE) {
	if (segments)
		stopifnot(missing(y))
	if (diagonal)
		stopifnot(! missing(y))
	missing.y = missing(y) # assigning y later on changes missing(y)
	if (is(x, "Spatial")) {
		if (! missing(y))
			stopifnot(identicalCRS(x, y))
		ll = !is.na(is.projected(x)) && !is.projected(x)
		if (!missing(longlat) && longlat != ll)
			warning(paste("spDists: argument longlat conflicts with CRS(x); using the value", longlat))
		else
			longlat = ll
		x = coordinates(x)
		y = coordinates(y)
	}
	stopifnot(ncol(x) == ncol(y))
	if (segments) {
		stopifnot(ncol(x) == 2)
		n = nrow(x)
		if (n == 1) return(0)
		res <- .C("sp_lengths", as.double(x[,1]), as.double(x[,2]), 
			as.integer(n), vector(mode = "double", length=(n-1)), 
			as.integer(longlat), PACKAGE = "sp")[[4]]
		if (any(!is.finite(res)))
			stop("non-finite segment lengths")
		res
	} else if (diagonal) {
		stopifnot(ncol(x) == 2)
		stopifnot(nrow(x) == nrow(y))
		res = .C("sp_dists_NN", as.double(x[,1]), as.double(x[,2]), 
			as.double(y[,1]), as.double(y[,2]), as.integer(nrow(x)),
			vector(mode = "double", length = nrow(x)), 
			as.integer(longlat), PACKAGE = "sp")[[6]]
		if (any(!is.finite(res)))
			stop("non-finite lengths")
		res
	} else if (ncol(x) != 2) {
		if (longlat)
			stop("cannot compute spherical distances for longlat data in more than 2 dimensions")
		if (missing.y)
			as.matrix(dist(x))
    	else
			sqrt(Reduce("+", Map(function(i) outer(x[,i], y[,i], "-") ^ 2, 1:ncol(x))))
	} else {
		spDiN1 = function(x, y, ll) spDistsN1(y, x, ll)
		if (nrow(x) < nrow(y))
			matrix(t(apply(x, 1, spDiN1, y = y, ll = longlat)), nrow(x), nrow(y))
		else
			matrix(apply(y, 1, spDiN1, y = x, ll = longlat), nrow(x), nrow(y))
	}
}
