flipHorizontal <- function(x) {
	if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a SpatialGridDataFrame")
	grd <- getGridTopology(x)
	idx = 1:prod(grd@cells.dim[1:2])
	m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[,grd@cells.dim[1]:1]
	idx = as.vector(t(m)) 
	x@data <- x@data[idx, TRUE, drop = FALSE]
	x
}

flipVertical <- function(x) {
	if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a SpatialGridDataFrame")
	grd <- getGridTopology(x)
	idx = 1:prod(grd@cells.dim[1:2])
	m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[grd@cells.dim[2]:1, ]
	idx = as.vector(t(m)) 
	x@data <- x@data[idx, TRUE, drop = FALSE]
	x
}
