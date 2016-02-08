"SpatialPointsDataFrame" = function(coords, data, coords.nrs = numeric(0), 
		proj4string = CRS(as.character(NA)), match.ID, bbox = NULL) {

	if (!is(coords, "SpatialPoints"))
		coords = coordinates(coords) 
		# make sure data.frame becomes double matrix; NA checks
	mtch = NULL
	cc.ID = row.names(coords) # row.names works for both matrix AND SpatialPoints
	if (missing(match.ID)) { # sort it out:
		if (is.null(cc.ID))
			match.ID = FALSE # nothing to match to!
		else {
			mtch = match(cc.ID, row.names(data))
			match.ID = !any(is.na(mtch)) # && length(unique(mtch)) == nrow(data)
			if (match.ID && any(mtch != 1:nrow(data)))
				warning("forming a SpatialPointsDataFrame based on maching IDs, not on record order. Use match.ID = FALSE to match on record order")
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
	if (!is(coords, "SpatialPoints"))
		coords = SpatialPoints(coords, proj4string = proj4string, bbox = bbox)
	# EJP, Tue Aug 13 19:54:04 CEST 2013
	if (is.character(attr(data, "row.names"))) # i.e., data has "real" row names
		dimnames(coords@coords)[[1]] = row.names(data)
	new("SpatialPointsDataFrame", coords, data = data, coords.nrs = coords.nrs)
}

setMethod("coordinates", "SpatialPointsDataFrame", function(obj) obj@coords)

setMethod("addAttrToGeom", signature(x = "SpatialPoints", y = "data.frame"),
	function(x, y, match.ID, ...) 
		SpatialPointsDataFrame(x, y, match.ID = match.ID, ...)
)

setReplaceMethod("coordinates", signature(object = "data.frame", value = "ANY"),
  function(object, value) {
	coord.numbers = NULL
	if (inherits(value, "formula")) {
		cc = model.frame(value, object, na.action = na.fail) # retrieve coords
		if (dim(cc)[2] == 2) {
			nm = as.character(as.list(value)[[2]])[2:3]
			coord.numbers = match(nm, names(object))
		} else if (dim(cc)[2] == 3) {
			nm = c(as.character(as.list((as.list(value)[[2]])[2])[[1]])[2:3],
				as.character(as.list(value)[[2]])[3])
			coord.numbers = match(nm, names(object))
		} # else: give up.
	} else if (is.character(value)) {
		cc = object[, value] # retrieve coords
		coord.numbers = match(value, names(object))
	} else if (is.null(dim(value)) && length(value) > 1) { # coord.columns?
		if (any(value != as.integer(value) || any(value < 1)))
			stop("coordinate columns should be positive integers")
		cc = object[, value] # retrieve coords
		coord.numbers = value
	} else  # raw coordinates given; try transform them to matrix:
		cc = coordinates(value)
	if (any(is.na(cc)))
		stop("coordinates are not allowed to contain missing values")
	if (!is.null(coord.numbers)) {
		object = object[ , -coord.numbers, drop = FALSE]
		stripped = coord.numbers
		# ... but as.data.frame(x) will merge them back in, so nothing gets lost.
		if (ncol(object) == 0)
			#stop("only coords columns present: use SpatialPoints to create a points object")
			return(SpatialPoints(cc))
	} else
		stripped = numeric(0)
	SpatialPointsDataFrame(coords = cc, data = object, coords.nrs = stripped,
		match.ID = FALSE)
  }
)

.asWKT = FALSE
print.SpatialPointsDataFrame = function(x, ..., digits = getOption("digits"), 
		asWKT = .asWKT) {
	#EJP, Fri May 21 12:40:59 CEST 2010
	if (asWKT)
		df = data.frame(asWKTSpatialPoints(x, digits), x@data)
	else { # old style
		cc = substring(paste(as.data.frame(
			t(signif(coordinates(x), digits)))),2,999)
		df = data.frame("coordinates" = cc, x@data)
	}
	row.names(df) = row.names(x@data)
	print(df, ..., digits = digits)
}
setMethod("show", "SpatialPointsDataFrame", function(object) print(object))

dim.SpatialPointsDataFrame = function(x) dim(x@data)

as.data.frame.SpatialPointsDataFrame = function(x, ...)  {
	if (length(x@coords.nrs) > 0) {
		maxi = max(x@coords.nrs, (ncol(x@data) + ncol(x@coords)))
		ret = list()
		for (i in 1:ncol(x@coords))
			ret[[x@coords.nrs[i]]] = x@coords[,i]
		names(ret)[x@coords.nrs] = dimnames(x@coords)[[2]]
		idx.new = (1:maxi)[-(x@coords.nrs)]
		for (i in 1:ncol(x@data))
			ret[[idx.new[i]]] = x@data[,i]
		names(ret)[idx.new] = names(x@data)
		ret = ret[unlist(lapply(ret, function(x) !is.null(x)))]
		data.frame(ret, ...)
	} else
		data.frame(x@data, x@coords, ...)
}

setAs("SpatialPointsDataFrame", "data.frame", function(from)
	as.data.frame.SpatialPointsDataFrame(from))

names.SpatialPointsDataFrame <- function(x) names(x@data)
"names<-.SpatialPointsDataFrame" <- function(x, value) { 
	checkNames(value)
	names(x@data) = value 
	x 
}

points.SpatialPointsDataFrame = function(x, y = NULL, ...) 
	points(as(x, "SpatialPoints"), ...)

text.SpatialPointsDataFrame = function(x, ...) {
	lst = list(...)
	if (!is.null(x$srt)) {
		if (length(unique(x$srt)) == 1)
			lst$srt = x$srt[1]
		else { # print each label individually:
			lapply(seq_len(length(x)), function(i) text(x[i,], ...))
			return(invisible())
		}
	}
    lst$x = coordinates(x)
    if (!is.null(x$pos) && is.null(lst$pos))
        lst$pos = x$pos
    if (!is.null(x$offset) && is.null(lst$offset))
        lst$offset = x$offset
    if (!is.null(x$labels) && is.null(lst$labels))
        lst$labels = parse(text = x$labels)
    if (!is.null(x$adjx) && !is.null(x$adjy))
        lst$adj = c(x$adjx, x$adjy)
    do.call(text, lst)
}

row.names.SpatialPointsDataFrame <- function(x) {
    ret = dimnames(slot(x, "coords"))[[1]]
	if (is.null(ret))
		ret = row.names(x@data)
	ret
}

"row.names<-.SpatialPointsDataFrame" <- function(x, value) {
    dimnames(slot(x, "coords"))[[1]] <- value
	rownames(slot(x, "data")) <- value
	x
}

setMethod("[", "SpatialPointsDataFrame", function(x, i, j, ..., drop = TRUE) {
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
		stop("matrix argument not supported in SpatialPointsDataFrame selection")
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (is.character(i)) 
		i <- match(i, row.names(x))
	if (any(is.na(i))) 
		stop("NAs not permitted in row index")
	if (!isTRUE(j)) # i.e., we do some sort of column selection
		x@coords.nrs = numeric(0) # will move coordinate colums last
	x@coords = x@coords[i, , drop = FALSE]
	if (nrow(x@coords))
		x@bbox = .bboxCoords(x@coords)
	x@data = x@data[i, j, ..., drop = FALSE]
	x
})

setMethod("split", "SpatialPointsDataFrame", split.data.frame)

setMethod("geometry", "SpatialPointsDataFrame",
	function(obj) as(obj, "SpatialPoints"))

length.SpatialPointsDataFrame = function(x) { nrow(x@coords) }
