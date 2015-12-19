zerodist <- function(obj, zero = 0.0, unique.ID = FALSE, memcmp = TRUE) {
	if (!extends(class(obj), "SpatialPoints"))
		stop("obj should be of, or extend, class SpatialPoints")
	lonlat = as.integer(!is.na(is.projected(obj)) && !is.projected(obj))
	cc = coordinates(obj)
	cmp = as.integer(memcmp)
	if (unique.ID) # return unique IDs only, not all zero dist pairs
		.Call(sp_duplicates, as.vector(t(cc)), ncol(cc), zero, lonlat, cmp) + 1
	else
		matrix(.Call(sp_zerodist, as.vector(t(cc)), ncol(cc), zero, 
			lonlat, cmp), ncol = 2, byrow = TRUE) + 1
}

zerodist2 <- function (obj1, obj2, zero = 0, memcmp = TRUE) {
    if (!(extends(class(obj1), "SpatialPoints")
    		&& extends(class(obj2), "SpatialPoints"))) 
        stop("obj1 and obj2 should be of, or extend, class SpatialPoints")
	stopifnot(identicalCRS(obj1, obj2))
	lonlat = as.integer(!is.na(is.projected(obj1)) && !is.projected(obj1))
	cmp = as.integer(memcmp)
    cc1 = coordinates(obj1)
    cc2 = coordinates(obj2)
	n = nrow(cc1)
	cc = rbind(cc1, cc2)
	ret = matrix(.Call(sp_zerodist, as.vector(t(cc)), ncol(cc), zero, 
		lonlat, cmp), ncol = 2, byrow = TRUE) + 1
	ret = ret[ret[,1] <= n & ret[,2] > n, , drop=FALSE]
	ret[,2] = ret[,2] - n
	ret
}

remove.duplicates <- function(obj, zero = 0.0, remove.second = TRUE, 
		memcmp = TRUE) {
	if (! remove.second) 
		obj = obj[length(obj):1,]
	zd = zerodist(obj, zero, unique.ID = TRUE, memcmp = memcmp)
	uq = zd == (1:length(obj))
	if (any(!uq)) { # if any non-unique points
		obj = obj[uq,] # select the unique ones
		if (! remove.second)
			obj = obj[length(obj):1,]
	} 
	obj
}
