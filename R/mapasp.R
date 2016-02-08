mapasp <- function(data, xlim = bbox(data)[1,], ylim = bbox(data)[2,]) {
	# calculates aspect ratio for levelplot of geographic data,
	# using proportial units (compare eqscplot)
	if (!is(data, "Spatial"))
		stop("cannot extract coordinates bounding box from data")
	if (!(is.na(proj4string(data)) || is.projected(data)))
		return( (diff(ylim)/diff(xlim)) / cos((mean(ylim) * pi)/180))
	if (is.R() && version$major >= 2)
		return("iso")
	else
		return(diff(ylim)/diff(xlim))
}
