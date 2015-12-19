setMethod("gridded", "Spatial", 
	function(obj) { is(obj, "SpatialPixels") || is(obj, "SpatialGrid") })

# grid -> points:
setReplaceMethod("gridded", c("SpatialGridDataFrame", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPointsDataFrame"); obj })
setReplaceMethod("gridded", c("SpatialPixelsDataFrame", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPointsDataFrame"); obj })
setReplaceMethod("gridded", c("SpatialGrid", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPoints"); obj })
setReplaceMethod("gridded", c("SpatialPixels", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPoints"); obj })

# points -> grid:
setReplaceMethod("gridded", c("SpatialPointsDataFrame", "logical"), 
	function(obj, value) { if (value) obj = as(obj, "SpatialPixelsDataFrame"); obj })
setReplaceMethod("gridded", c("SpatialPoints", "logical"), 
	function(obj, value) { if (value) obj = as(obj, "SpatialPixels"); obj })

# points -> grid, with grid specified as list(value, grid):
setReplaceMethod("gridded", c("SpatialPointsDataFrame", "list"), 
	function(obj, value) { if (value[[1]]) obj = SpatialPixelsDataFrame(obj, obj@data, grid = value[[2]]); obj })
setReplaceMethod("gridded", c("SpatialPoints", "list"), 
	function(obj, value) { if (value[[1]]) obj = SpatialPixels(obj, grid = value[[2]]); obj })

# data.frame -> gridded:
setReplaceMethod("gridded", c("data.frame", "formula"),
	function(obj, value) { coordinates(obj) = value; gridded(obj) = TRUE; obj })
setReplaceMethod("gridded", c("data.frame", "character"),
	function(obj, value) { coordinates(obj) = value; gridded(obj) = TRUE; obj })

# data.frame -> gridded, grid specified:
setReplaceMethod("gridded", c("data.frame", "GridTopology"),
	function(obj, value) SpatialGridDataFrame(grid = SpatialGrid(grid = value), data.frame(obj)))

setAs("SpatialPoints", "SpatialPixels", function(from) {
	SpatialPixels(from, grid = NULL)
})

setAs("SpatialPointsDataFrame", "SpatialPixelsDataFrame", function(from) {
	SpatialPixelsDataFrame(from, from@data, grid = NULL)
})

setMethod("fullgrid", c("Spatial"), function(obj) is(obj, "SpatialGrid"))

setReplaceMethod("fullgrid", c("SpatialPixels", "logical"), 
	function(obj, value) { if(value) obj = as(obj, "SpatialGrid"); obj })
setReplaceMethod("fullgrid", c("SpatialGrid", "logical"), 
	function(obj, value) { if(!value) obj = as(obj, "SpatialPixels"); obj })
setReplaceMethod("fullgrid", c("SpatialPixelsDataFrame", "logical"), 
	function(obj, value) { if(value) obj = as(obj, "SpatialGridDataFrame"); obj })
setReplaceMethod("fullgrid", c("SpatialGridDataFrame", "logical"), 
	function(obj, value) { if(!value) obj = as(obj, "SpatialPixelsDataFrame"); obj })
