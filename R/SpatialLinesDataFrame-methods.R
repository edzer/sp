SpatialLinesDataFrame = function(sl, data, match.ID = TRUE) {
    if (is.character(match.ID)) {
        row.names(data) = data[, match.ID[1]]
        match.ID = TRUE
    }
	if (match.ID) {
		Sl_IDs <- sapply(slot(sl, "lines"), function(x) slot(x, "ID"))
		data_IDs <- row.names(data)
		mtch <- match(Sl_IDs, data_IDs)
		if (any(is.na(mtch)))
			stop("row.names of data and Lines IDs do not match")
		if (length(unique(mtch)) != length(Sl_IDs))
			stop("row.names of data and Lines IDs do not match")
		data <- data[mtch, , drop=FALSE]
	}
	if (nrow(data) != length(sl@lines))
		stop("length of data.frame does not match number of Lines elements")
	new("SpatialLinesDataFrame", sl, data = data)
}

names.SpatialLinesDataFrame = function(x) names(x@data)
"names<-.SpatialLinesDataFrame" = function(x,value) { checkNames(value); names(x@data)<-value; x }

as.data.frame.SpatialLinesDataFrame = function(x, row.names, optional, ...) x@data

setMethod("addAttrToGeom", signature(x = "SpatialLines", y = "data.frame"),
	function(x, y, match.ID, ...)
		SpatialLinesDataFrame(x, y, match.ID = match.ID, ...)
)

setAs("SpatialLinesDataFrame", "SpatialMultiPointsDataFrame", 
	function(from)
		SpatialMultiPointsDataFrame(as(geometry(from), "SpatialMultiPoints"), from@data)
)
setAs("SpatialLinesDataFrame", "data.frame", function(from)
    as.data.frame.SpatialLinesDataFrame(from))

row.names.SpatialLinesDataFrame <- function(x) {
    sapply(slot(x, "lines"), slot, "ID")
}

"row.names<-.SpatialLinesDataFrame" <- function(x, value) {
    spChFIDs(x, value)
}

setMethod("[", c("SpatialLinesDataFrame", "ANY", "ANY"), function(x, i, j, ... , drop = TRUE) {
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
        stop("matrix argument not supported in SpatialLinesDataFrame selection")
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
    if (is.logical(i)) {
	if (length(i) == 1 && i)
	    i = 1:length(x@lines)
	else
	    i <- which(i)
    } else if (is.character(i)) {
            i <- match(i, row.names(x))
    }
    if (any(is.na(i)))
		stop("NAs not permitted in row index")
    #SpatialLinesDataFrame(as(x, "SpatialLines")[i, , drop = FALSE],
    #    data = x@data[i, j, drop = FALSE], match.ID = FALSE)
	x@lines = x@lines[i]
	x@data = x@data[i, j, ..., drop = FALSE]
# RSB 081003
	if (length(x@lines) > 0) # EJP, 151218
		x@bbox = .bboxSls(x@lines)
	x
})

lines.SpatialLinesDataFrame = function(x, y = NULL, ...) 
	lines(as(x, "SpatialLines"), ...)

setAs("SpatialLinesDataFrame", "SpatialPointsDataFrame", function(from) {
		spp = as(as(from, "SpatialLines"), "SpatialPointsDataFrame")
		dfl = from@data[spp$Lines.NR, , drop = FALSE]
		spp@data = cbind(dfl, spp@data)
		spp
	}
)

dim.SpatialLinesDataFrame = function(x) dim(x@data)

setMethod("split", "SpatialLinesDataFrame", split.data.frame)

print.SpatialLinesDataFrame = function(x, ..., digits = getOption("digits"), 
		asWKT = .asWKT)
	print(data.frame(asWKTSpatialLines(x, digits), x@data),..., digits = digits)

setMethod("geometry", "SpatialLinesDataFrame",
	function(obj) as(obj, "SpatialLines"))

length.SpatialLinesDataFrame = function(x) { length(x@lines) }
