"SpatialPoints" = function(coords, proj4string = CRS(as.character(NA)),
        bbox = NULL) {
	coords = coordinates(coords) # checks numeric mode
	colNames = dimnames(coords)[[2]]
	if (is.null(colNames))
		colNames = paste("coords.x", 1:(dim(coords)[2]), sep = "")
	rowNames = dimnames(coords)[[1]]
	dimnames(coords) = list(rowNames, colNames) # preserve row names if non-NULL
	if (is.null(bbox)) 
		bbox <- .bboxCoords(coords)
	new("SpatialPoints", coords = coords, bbox = bbox,
		proj4string = proj4string) # transpose bbox?
}

.bboxCoords = function(coords) {
	stopifnot(nrow(coords) > 0)
	bbox = t(apply(coords, 2, range))
	dimnames(bbox)[[2]] = c("min", "max")
	as.matrix(bbox)
}

setMethod("coordinates", "matrix", 
	function(obj) {
		if (!is.numeric(obj))
			stop("cannot derive coordinates from non-numeric matrix")
		storage.mode(obj) <- "double"
		if (any(is.na(obj)))
			stop("NA values in coordinates")
		if (any(!is.finite(obj)))
			stop("non-finite coordinates")
		obj
	}
)
setMethod("coordinates", "data.frame", function(obj)coordinates(as.matrix(obj)))

setMethod("coordinates", "list", function(obj) coordinates(as.data.frame(obj)))

asWKTSpatialPoints = function(x, digits = getOption("digits")) {
	data.frame(geometry = paste("POINT(",unlist(lapply(data.frame(
		t(signif(coordinates(x),digits = digits))),
		paste, collapse=" ")),")",sep=""))
}

"print.SpatialPoints" <- function(x, ..., digits = getOption("digits"), 
	asWKT = .asWKT)
{
	cat("SpatialPoints:\n")
	if (asWKT) 
		print(asWKTSpatialPoints(x, digits))
	else
		print(x@coords)
	pst <- paste(strwrap(paste(
		"Coordinate Reference System (CRS) arguments:", 
		proj4string(x))), collapse="\n")
	cat(pst, "\n")
}
setMethod("show", "SpatialPoints", function(object) print.SpatialPoints(object))

plot.SpatialPoints = function(x, pch = 3, axes = FALSE, add = FALSE, 
	xlim = NULL, ylim = NULL, ..., setParUsrBB=FALSE, cex = 1, col = 1, 
	lwd = 1, bg = 1) 
{
	if (! add)
		plot(as(x, "Spatial"), axes = axes, xlim = xlim, ylim = ylim, 
			..., setParUsrBB=setParUsrBB)
	cc = coordinates(x)
	points(cc[,1], cc[,2], pch = pch, cex = cex, col = col, lwd = lwd, bg = bg)
}
setMethod("plot", signature(x = "SpatialPoints", y = "missing"),
	function(x,y,...) plot.SpatialPoints(x,...))

points.SpatialPoints = function(x, y = NULL, ...) points(coordinates(x), ...)

setMethod("coordinates", "SpatialPoints", function(obj) obj@coords)

as.data.frame.SpatialPoints = function(x, row.names, optional, ...) data.frame(x@coords)

setAs("SpatialPoints", "data.frame", function(from) as.data.frame(from))

row.names.SpatialPoints <- function(x) {
    ret = dimnames(slot(x, "coords"))[[1]]
	if (is.null(ret))
		seq_len(nrow(slot(x, "coords")))
	else 
		ret
}

"row.names<-.SpatialPoints" <- function(x, value) {
    dimnames(slot(x, "coords"))[[1]] <- value
	x
}

setMethod("[", "SpatialPoints", function(x, i, j, ..., drop = TRUE) {
	if (!missing(j))
		warning("j index ignored")
	if (is.character(i))
		i <- match(i, row.names(x))
	else if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (any(is.na(i)))
		stop("NAs not permitted in row index")
	x@coords = x@coords[i, , drop = FALSE]
	if (drop && nrow(x@coords))
		x@bbox = .bboxCoords(x@coords)
	x
})

setMethod("coordnames", signature(x = "SpatialPoints"),
	function(x) dimnames(x@coords)[[2]])

setReplaceMethod("coordnames", signature(x = "SpatialPoints", value = "character"),
	function(x, value) {
		dimnames(x@bbox)[[1]] = value
		dimnames(x@coords)[[2]] = value
		x
	}
)

length.SpatialPoints = function(x) { nrow(x@coords) }

setMethod("$", "SpatialPoints", 
	function(x, name) {
		if (name %in% coordnames(x))
			return(x@coords[,name])
		if (!("data" %in% slotNames(x)))
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)
