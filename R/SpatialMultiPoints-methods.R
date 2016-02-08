"SpatialMultiPoints" = function(coords, proj4string = CRS(as.character(NA)),
        bbox = NULL) {
	coords = lapply(coords, coordinates) # checks numeric mode
	#colNames = dimnames(coords[[1]])[[2]]
	#if (is.null(colNames))
	#	colNames = paste("coords.x", 1:(dim(coords)[2]), sep = "")
	#rowNames = dimnames(coords)[[1]]
	#dimnames(coords) = list(rowNames, colNames) # preserve row names if non-NULL
	if (is.null(bbox)) 
		bbox <- .bboxMultiCoords(coords)
	new("SpatialMultiPoints", coords = coords, bbox = bbox, proj4string = proj4string)
}

.bboxMultiCoords = function(coords) {
	coords = do.call(rbind, coords)
	stopifnot(nrow(coords) > 0)
	bbox = t(apply(coords, 2, range))
	dimnames(bbox)[[2]] = c("min", "max")
	as.matrix(bbox)
}

asWKTSpatialMultiPoints = function(x, digits = getOption("digits")) {
	data.frame(geometry = paste("MULTIPOINT (",
		sapply(x@coords, function(x)
		paste(apply(signif(x, digits = digits), 1, paste, collapse = " "), collapse = ","))
	, ")" , sep = ""))
}

"print.SpatialMultiPoints" <- function(x, ..., digits = getOption("digits"), 
	asWKT = .asWKT)
{
	cat("SpatialMultiPoints:\n")
	if (asWKT) 
		print(asWKTSpatialMultiPoints(x, digits))
	else
		print(x@coords)
	pst <- paste(strwrap(paste(
		"Coordinate Reference System (CRS) arguments:", 
		proj4string(x))), collapse="\n")
	cat(pst, "\n")
}
setMethod("show", "SpatialMultiPoints", function(object) print.SpatialMultiPoints(object))

plot.SpatialMultiPoints = function(x, pch = 3, axes = FALSE, add = FALSE, 
	xlim = NULL, ylim = NULL, ..., setParUsrBB=FALSE, cex = 1, col = 1, 
	lwd = 1, bg = 1) 
{
	if (! add)
		plot(as(x, "Spatial"), axes = axes, xlim = xlim, ylim = ylim, 
			..., setParUsrBB=setParUsrBB)
	cc = coordinates(x)
	n = length(x)
	l = sapply(x@coords, nrow)
	points(cc[,1], cc[,2], 
		pch = rep(rep(pch, length.out = n), l), 
		cex = rep(rep(cex, length.out = n), l), 
		col = rep(rep(col, length.out = n), l), 
		lwd = rep(rep(lwd, length.out = n), l), 
		bg = rep(rep(bg, length.out = n), l))
}
setMethod("plot", signature(x = "SpatialMultiPoints", y = "missing"),
	function(x,y,...) plot.SpatialMultiPoints(x,...))

points.SpatialMultiPoints = function(x, y = NULL, ...) plot(x, add = TRUE, ...)

setMethod("coordinates", "SpatialMultiPoints", 
	function(obj) {
		if (length(obj@coords) == 0)
			matrix(nrow = 0, ncol = 2)
		else
			matrix(do.call(rbind, obj@coords), ncol = ncol(obj@coords[[1]]),
				dimnames = list(rep(1:length(obj@coords), sapply(obj@coords, nrow))))
	}
)

as.data.frame.SpatialMultiPoints = function(x, row.names, optional, ...) data.frame(coordinates(x@coords))

setAs("SpatialMultiPoints", "data.frame", function(from) as.data.frame(from))

row.names.SpatialMultiPoints <- function(x) {
    ret = names(slot(x, "coords"))
	if (is.null(ret))
		ret = seq_len(slot(x, "coords"))
	ret
}
names.SpatialMultiPoints <- row.names.SpatialMultiPoints 

"row.names<-.SpatialMultiPoints" <- function(x, value) {
    names(slot(x, "coords")) <- value
	x
}

setMethod("[", "SpatialMultiPoints", function(x, i, j, ..., drop = TRUE) {
	if (!missing(j))
		warning("j index ignored")
	if (is.character(i))
		i <- match(i, row.names(x))
	else if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (any(is.na(i)))
		stop("NAs not permitted in row index")
	x@coords = x@coords[i]
	if (drop && length(x@coords))
		x@bbox = .bboxMultiCoords(x@coords)
	x
})

setMethod("coordnames", signature(x = "SpatialMultiPoints"),
	function(x) dimnames(x@coords[[1]])[[2]])

setReplaceMethod("coordnames", signature(x = "SpatialMultiPoints", value = "character"),
	function(x, value) {
		dimnames(x@bbox)[[1]] = value
		dimnames(x@coords[[1]])[[2]] = value
		x
	}
)

length.SpatialMultiPoints = function(x) length(x@coords)

setMethod("$", "SpatialMultiPoints", 
	function(x, name) {
		if (name %in% coordnames(x))
			return(SpatialMultiPoints(lapply(x@coords, function(x) x[,name])))
		if (!("data" %in% slotNames(x))) # we're not a SpatialMultiPointsDataFrame
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)

setAs("SpatialMultiPoints", "SpatialPoints", 
	function(from) {
		cc = coordinates(from)
		attr(cc, "groupIndex") = dimnames(cc)[[1]]
		dimnames(cc)[[1]] = NULL # if we'd retain them, rgeos would interpret the SpatialPoints as SpatialMultiPoints
		SpatialPoints(cc, from@proj4string, from@bbox)
	}
)

split.SpatialPoints = function(x, f, drop = FALSE, ...) {
	lst = lapply(split(as.data.frame(coordinates(x)), f), as.matrix)
	SpatialMultiPoints(lst, x@proj4string, x@bbox)
}
