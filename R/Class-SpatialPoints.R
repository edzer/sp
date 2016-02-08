setClass("SpatialPoints",
	contains = "Spatial", 
	slots = c(coords = "matrix"),
	prototype = list(bbox = matrix(NA), 
		proj4string = CRS(as.character(NA)),
		coords = matrix(0)),
	validity = function(object) {
		if (!is.matrix(object@coords))
			return("coords slot is not a matrix")
		if (ncol(object@coords) < 2)
			return("SpatialPoints: too few coordinate columns")
		if (!is.double(object@coords[,1]))
			return("coordinates should be double")
		rowNames = dimnames(object@coords)[[1]]
		if (!is.null(rowNames) && anyDuplicated(rowNames))
			warning("duplicate rownames are interpreted by rgeos as MultiPoints; use SpatialMultiPoints to define these; in future sp versions this warning will become an error")
		return(TRUE)
	}
)
