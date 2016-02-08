read.asciigrid <- function(fname, as.image = FALSE, plot.image = FALSE,
	colname=fname, proj4string = CRS(as.character(NA))) {
	t = file(fname, "r")
	l5 = readLines(t, n = 6)
	# instead of:
	# l5s = strsplit(l5, " ")
	# Michael Summer, mdsumner@utas.edu.au suggested:
	l5s = strsplit(l5, "\\s+", perl = T)
	xllcenter = yllcenter = xllcorner = yllcorner = as.numeric(NA)
	for (i in 1:6) {
		fieldname = casefold(l5s[[i]][1])
		if (length(grep("ncols", fieldname)))
			ncols = as.numeric(l5s[[i]][2])
		if (length(grep("nrows", fieldname)))
			nrows = as.numeric(l5s[[i]][2])
		if (length(grep("xllcorner", fieldname)))
			xllcorner = as.numeric(l5s[[i]][2])
		if (length(grep("yllcorner", fieldname)))
			yllcorner = as.numeric(l5s[[i]][2])
		if (length(grep("xllcenter", fieldname)))
			xllcenter = as.numeric(l5s[[i]][2])
		if (length(grep("yllcenter", fieldname)))
			yllcenter = as.numeric(l5s[[i]][2])
		if (length(grep("cellsize", fieldname)))
			cellsize = as.numeric(l5s[[i]][2])
		if (length(grep("nodata_value", fieldname)))
			nodata.value = as.numeric(l5s[[i]][2])
	}
	if (is.na(xllcorner) && !is.na(xllcenter))
		xllcorner = xllcenter - 0.5 * cellsize
	else
		xllcenter = xllcorner + 0.5 * cellsize
	if (is.na(yllcorner) && !is.na(yllcenter))
		yllcorner = yllcenter - 0.5 * cellsize
	else
		yllcenter = yllcorner + 0.5 * cellsize
	map = scan(t, as.numeric(0), quiet = TRUE)
	close(t)
	if (length(as.vector(map)) != nrows * ncols)
		stop("dimensions of map do not match that of header")
	map[map == nodata.value] = NA
	if (as.image) {
		img = matrix(map, ncols, nrows)[,nrows:1]
		img = list(z = img, x = xllcorner + cellsize * ((1:ncols) - 0.5),
				y = yllcorner + cellsize * ((1:nrows) - 0.5))
		if (plot.image) {
			image(img, asp = 1)
			return(invisible(img))
		} else
			return(img)
	} 
	df = data.frame(map)
	names(df) = colname
	grid = GridTopology(c(xllcenter,yllcenter), rep(cellsize,2), c(ncols,nrows))
	SpatialGridDataFrame(grid, data = df, proj4string=proj4string)
}

write.asciigrid <- function(x, fname, attr = 1, na.value = -9999, ...) { 

# R> gridparameters(meuse.grid)
#   cellcentre.offset cellsize cells.dim
# x            178460       40        78
# y            329620       40       104

#NCOLS 80
#NROWS 115
#XLLCORNER 178400.000000
#YLLCORNER 329400.000000
#CELLSIZE 40.000000
#NODATA_VALUE 1e31

	if (!gridded(x))
		stop("can only write SpatialGridDataFrame objects to asciigrid")
	x = as(x, "SpatialGridDataFrame")
	gp = gridparameters(x)
	if (length(gp$cells.dim) != 2)
		stop("asciigrid only supports 2D grids")
	if (gp$cellsize[1] != gp$cellsize[2])
		stop("Asciigrid does not support grids with non-square cells")
	f = file(fname, open = "w")
	writeLines(c(
		paste("NCOLS", gp$cells.dim[1]),
		paste("NROWS", gp$cells.dim[2]),
		paste("XLLCORNER", gp$cellcentre.offset[1] - 0.5 * gp$cellsize[1]),
		paste("YLLCORNER", gp$cellcentre.offset[2] - 0.5 * gp$cellsize[2]),
		paste("CELLSIZE", gp$cellsize[1]),
		paste("NODATA_VALUE", na.value)
	  ), f)
	z = x@data[[attr]]
	z[is.na(z)] = na.value
	write.table(t(matrix(z, gp$cells.dim[1], gp$cells.dim[2])), file = f, 
			row.names = FALSE, col.names = FALSE, ...)
	close(f)
}
