setClass("SpatialMultiPoints",
	contains = "Spatial", 
	slots = c(coords = "list"),
	prototype = list(bbox = matrix(NA), 
		proj4string = CRS(as.character(NA)),
		coords = list()),
	validity = function(object) {
		if (!is.list(object@coords))
			return("coords slot is not a list")
		if (!all(sapply(object@coords, is.matrix)))
			return("all list elements need to be a matrix")
		if (!all(sapply(object@coords, is.double)))
			return("all coordinates must be of mode double")
		if (unique(sapply(object@coords, ncol)) == 1)
			return("all coordinate elements need to have the same number of columns")
		return(TRUE)
	}
)
