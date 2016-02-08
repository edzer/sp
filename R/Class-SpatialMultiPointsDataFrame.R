setClass("SpatialMultiPointsDataFrame",
	contains = "SpatialMultiPoints", 
	slots = c(data = "data.frame"),
	prototype = list(bbox = matrix(NA), proj4string = CRS(as.character(NA)),
		coords = list(), data = data.frame()),
	validity = function(object) {
		if (nrow(object@data) != length(object@coords))
			return("number of rows in data.frame and point sets in SpatialMultiPoints don't match")
		return(TRUE)
	}
)
