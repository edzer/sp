"spmap.to.lev" <- function (data, zcol = 1:n, n = 2, names.attr)
{
	if (!(is(data, "SpatialPointsDataFrame") || (is(data, "SpatialGridDataFrame"))))
		stop("data is not of a class that extends SpatialPointsDataFrame")

	if (dimensions(data) > 2) {
		warning("spmap.to.lev ignores spatial dimensions beyond the first 2")
		cc = coordinates(data)[,1:2]
		data = as(data, "data.frame")
		coordinates(data) = cc
	}
	coord.names = dimnames(data@coords)[[2]]

	if (missing(names.attr)) {
		if (is.character(zcol))
			names.attr = zcol
		else {
			names.attr = names(data)[zcol]
			zcol = names.attr
		}
	} else {
		if (length(names.attr) != length(zcol))
			stop("length names.attr should match length of zcol")
		if (!is.character(zcol))
			zcol = names(data)[zcol]
	}

	data = stack(as(data, "SpatialPointsDataFrame"), zcol) # replace with data.frame
	#data$ind = factor(as.character(data$ind), levels = zcol, labels = names.attr)
	# Arien Lam suggested:
	#data$ind = factor(data$ind, levels = unique(data$ind), labels = names.attr)
	# better (as it avoids unique()) is:
	data$ind = factor(data$ind, levels = zcol, labels = names.attr)
	names(data) = c(coord.names, "z", "name")
	data
}

stack.SpatialPointsDataFrame = function (x, select, ...)
{
	lev = NULL
	xd = x@data
   	cc = coordinates(x)
	cc.names = dimnames(cc)[[2]]

	if (!missing(select))
		xd = xd[select]
	if (is.factor(xd[[1]])) {
		lev = levels(xd[[1]])
		if (length(xd) > 1)
			for (i in 2:length(xd))
				if (!identical(lev, levels(xd[[i]])))
					stop("all factors should have identical levels")
	}
	nc = ncol(xd)
	xd = stack(data.frame(lapply(xd, as.numeric)))
	if (!is.null(lev))
		xd[[1]] = factor(lev[xd[[1]]], levels = lev)

	ccr = data.frame(rep(cc[,1], nc))
	for (i in 2:ncol(cc))
		ccr = data.frame(ccr, rep(cc[,i], nc))
	names(ccr) = cc.names
	data.frame(ccr, xd)
}

stack.SpatialGridDataFrame = function (x, select, ...)
	stack(as(x, "SpatialPointsDataFrame"), select, ...)

stack.SpatialPixelsDataFrame = function (x, select, ...)
	stack(as(x, "SpatialPointsDataFrame"), select, ...)
