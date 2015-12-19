setClass("SpatialPointsDataFrame",
	contains = "SpatialPoints", 
	slots = c(data = "data.frame", coords.nrs = "numeric"),
	prototype = list(bbox = matrix(NA), proj4string = CRS(as.character(NA)),
		coords = matrix(NA), data = data.frame(), 
		coords.nrs = numeric(0)),
	validity = function(object) {
		# if (ncol(object@data) == 0)
		# 	stop("data.frame is empty (possibly after stripping coordinate columns): use SpatialPoints() to create points-only object")
		if (nrow(object@data) != nrow(object@coords))
			return("number of rows in data.frame and SpatialPoints don't match")
		n <- length(object@coords.nrs)
		if (n > 0 && n != ncol(object@coords))
			return("inconsistent coords.nrs slot")
		return(TRUE)
	}
)
