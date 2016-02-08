"SpatialMultiPointsDataFrame" = function(coords, data,
		proj4string = CRS(as.character(NA)), match.ID, bbox = NULL) {

	if (!is(coords, "SpatialMultiPoints"))
		coords = SpatialMultiPoints(coords, proj4string = proj4string, bbox = bbox)
	mtch = NULL
	cc.ID = names(coords@coords)
	if (missing(match.ID)) { # sort it out:
		if (is.null(cc.ID) || any(cc.ID == ""))
			match.ID = FALSE # nothing to match to!
		else {
			mtch = match(cc.ID, row.names(data))
			match.ID = !any(is.na(mtch)) # && length(unique(mtch)) == nrow(data)
			if (match.ID && any(mtch != 1:nrow(data)))
				warning("forming a SpatialMultiPointsDataFrame based on maching IDs, not on record order. Use match.ID = FALSE to match on record order")
		}
	} else if (is.character(match.ID)) {
        row.names(data) = data[, match.ID[1]]
        match.ID = TRUE
    } 

	if (match.ID) {
		if (!is.null(cc.ID) && is(data, "data.frame")) { # match ID:
			if (is.null(mtch))
				mtch = match(cc.ID, row.names(data))
			if (any(is.na(mtch)))
				stop("row.names of data and coords do not match")
			if (length(unique(mtch)) != nrow(data))
				stop("row.names of data and dimnames of coords do not match")
			data = data[mtch, , drop = FALSE]
		}
	}
	if (is.character(attr(data, "row.names"))) # i.e., data has "real" row names
		names(coords@coords) = row.names(data)
	new("SpatialMultiPointsDataFrame", coords, data = data)
}

# setMethod("coordinates", "SpatialMultiPointsDataFrame", function(obj) obj@coords)

setMethod("addAttrToGeom", signature(x = "SpatialMultiPoints", y = "data.frame"),
	function(x, y, match.ID, ...) 
		SpatialMultiPointsDataFrame(x, y, match.ID = match.ID, ...)
)

.asWKT = FALSE
print.SpatialMultiPointsDataFrame = function(x, ..., digits = getOption("digits"), 
		asWKT = .asWKT) {
	if (asWKT)
		df = data.frame(asWKTSpatialMultiPoints(x, digits), x@data)
	else { # old style
		cc = substring(paste(as.data.frame(
			t(signif(coordinates(x), digits)))),2,999)
		ix = rep(seq_along(x@coords), sapply(x@coords, nrow))
		df = data.frame("coordinates" = cc, x@data[ix, , drop=FALSE])
	}
	print(df, ..., digits = digits)
}
setMethod("show", "SpatialMultiPointsDataFrame", function(object) print(object))

dim.SpatialMultiPointsDataFrame = function(x) dim(x@data)

as.data.frame.SpatialMultiPointsDataFrame = function(x, ...)  {
	l = sapply(x@coords, nrow)
	ix = rep(1:length(l), l)
	data.frame(coordinates(x), index = ix, x@data[ix,,drop=FALSE], ...)
}

setAs("SpatialMultiPointsDataFrame", "data.frame", function(from)
	as.data.frame.SpatialMultiPointsDataFrame(from))

names.SpatialMultiPointsDataFrame <- function(x) names(x@data)
"names<-.SpatialMultiPointsDataFrame" <- function(x, value) { 
	checkNames(value)
	names(x@data) = value 
	x 
}

points.SpatialMultiPointsDataFrame = function(x, y = NULL, ...) 
	points(as(x, "SpatialMultiPoints"), ...)

text.SpatialMultiPointsDataFrame = function(x, ...) {
    lst = list(x = coordinates(x), ...)
    if (!is.null(x$pos) && is.null(lst$pos))
        lst$pos = x$pos
    if (!is.null(x$offset) && is.null(lst$offset))
        lst$offset = x$offset
    if (!is.null(x$labels) && is.null(lst$labels))
        lst$labels = parse(text = x$lab)
    do.call(text, lst)
}

#row.names.SpatialMultiPointsDataFrame <- function(x) {
#    ret = dimnames(slot(x, "coords"))[[1]]
#	if (is.null(ret))
#		ret = row.names(x@data)
#	ret
#}

#"row.names<-.SpatialMultiPointsDataFrame" <- function(x, value) {
#    dimnames(slot(x, "coords"))[[1]] <- value
#	rownames(slot(x, "data")) <- value
#	x
#}

setMethod("[", "SpatialMultiPointsDataFrame", function(x, i, j, ..., drop = TRUE) {
	missing.i = missing(i)
	missing.j = missing(j)
	nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
	if (missing.i && missing.j) {
		i = TRUE
		j = TRUE
	} else if (missing.j && !missing.i) { 
		if (nargs == 2) {
			j = i
			i = TRUE
		} else {
			j = TRUE
		}
	} else if (missing.i && !missing.j)
		i = TRUE
	if (is.matrix(i))
		stop("matrix argument not supported in SpatialMultiPointsDataFrame selection")
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (is.character(i)) 
		i <- match(i, row.names(x))
	if (any(is.na(i))) 
		stop("NAs not permitted in row index")
	x@coords = x@coords[i]
	if (length(x@coords))
		x@bbox = .bboxMultiCoords(x@coords)
	x@data = x@data[i, j, ..., drop = FALSE]
	x
})

# setMethod("split", "SpatialMultiPointsDataFrame", split.data.frame)

setMethod("geometry", "SpatialMultiPointsDataFrame",
	function(obj) as(obj, "SpatialMultiPoints"))

length.SpatialMultiPointsDataFrame = function(x) { length(x@coords) }

setAs("SpatialMultiPointsDataFrame", "SpatialPointsDataFrame",
	function(from) {
		l = sapply(from@coords, nrow)
		ix = rep(1:length(l), l)
		new("SpatialPointsDataFrame", as(geometry(from), "SpatialPoints"),
			data = from@data[ix,,drop=FALSE], coords.nrs = numeric(0))
	}
)
