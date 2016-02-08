setClass("SpatialPixels", 
	contains = "SpatialPoints",
	slots = c(grid = "GridTopology", grid.index = "integer"),
	validity = function(object) {
		if (nrow(object@coords) != length(object@grid.index))
			return("grid.index should have length equal to nrow(coords)")
		if (length(object@grid.index) > 0) { 
			if (max(object@grid.index) > .NumberOfCells(object@grid))
				return("grid.index max value too large")
			if (min(object@grid.index) < 0)
				return("grid.index min value too small")
		}
		return(TRUE)
	}
)

setClass("SpatialGrid",
	contains = "Spatial", 
	slots = c(grid = "GridTopology"),
	validity = function(object) {
		return(TRUE)
	}
)
