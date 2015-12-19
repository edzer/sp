setClass("SpatialLinesDataFrame",
	contains = "SpatialLines", 
	slots = c(data = "data.frame"),
	validity = function(object) {
		if (!inherits(object@data, "data.frame"))
			stop("data should be of class data.frame")
		if (nrow(object@data) != length(object@lines))
		  stop("number of rows in data.frame and SpatialLines don't match")
		return(TRUE)
	}
)

as.SpatialLines.SLDF <- function(SLDF) SpatialLines(SLDF@lines)
