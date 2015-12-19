setClass("SpatialPolygonsDataFrame",
	contains = "SpatialPolygons", 
	slots = c(data = "data.frame"),
	validity = function(object) {
		if (!inherits(object@data, "data.frame"))
			stop("data should be of class data.frame")
		if (nrow(object@data) != length(object@polygons))
		  stop("number of rows in data.frame and polygons in SpatialPolygons don't match")
		return(TRUE)
	}
)

