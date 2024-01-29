mapasp <- function(data, xlim = bbox(data)[1,], ylim = bbox(data)[2,]) {
	# calculates aspect ratio for levelplot of geographic data,
	# using proportial units (compare eqscplot)
	if (!is(data, "Spatial"))
		stop("cannot extract coordinates bounding box from data")
	if (!(is.na(slot(slot(data, "proj4string"), "projargs")) || is.projected(data)))
		(diff(ylim)/diff(xlim)) / cos((mean(ylim) * pi)/180)
	else 
		diff(ylim)/diff(xlim)
}
