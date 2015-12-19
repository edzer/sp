SpatialPolygonsDataFrame <- function(Sr, data, match.ID = TRUE) {
# Barry comment 110610
        if (length(Sr@polygons) != nrow(data))
          stop(paste("Object length mismatch:\n    ", deparse(substitute(Sr)),
            "has", length(Sr@polygons), "Polygons objects, but",
            deparse(substitute(data)), "has", nrow(data), "rows", sep=" "))
	if (is.character(match.ID)) {
		row.names(data) = data[, match.ID[1]]
		match.ID = TRUE
	}
	if (match.ID) {
#		Sr_IDs <- sapply(slot(Sr, "polygons"),
#                    function(i) slot(i, "ID"))
                Sr_IDs <- .Call(SpatialPolygons_getIDs_c, Sr)
		data_IDs <- row.names(data)
		mtch <- match(Sr_IDs, data_IDs)
                if (!identical(Sr_IDs, data_IDs)) {
		    if (any(is.na(mtch)))
			stop("row.names of data and Polygons IDs do not match")
		    if (length(unique(mtch)) != length(Sr_IDs))
			stop("row.names of data and Polygons IDs do not match")
		   data <- data[mtch, , drop = FALSE]
               }
	}
	res <- new("SpatialPolygonsDataFrame")
        res@bbox <- Sr@bbox
        res@proj4string <- Sr@proj4string
        res@plotOrder <- Sr@plotOrder
        res@data <- data
        res@polygons <- Sr@polygons
# 120416 add top-level comment to reduce comment checking
        cSr <- comment(Sr)
        if (is.null(cSr))
            comment(res) <- as.character(any(sapply(slot(res, "polygons"),
                function(x) !is.null(comment(x))), na.rm=TRUE))
        else {
            if (!is.character(cSr) || is.na(cSr) || length(cSr) != 1)
            cSr <- as.character(any(sapply(slot(res, "polygons"),
                function(x) !is.null(comment(x))), na.rm=TRUE))
            comment(res) <- cSr
        }
        res
}

setReplaceMethod("polygons", signature(object = "data.frame", value = "SpatialPolygons"),
	function(object, value) SpatialPolygonsDataFrame(value, object))

setMethod("polygons", signature(obj = "SpatialPolygons"),
	function(obj) as(obj, "SpatialPolygons"))

setMethod("addAttrToGeom", signature(x = "SpatialPolygons", y = "data.frame"),
	function(x, y, match.ID, ...) 
		SpatialPolygonsDataFrame(x, y, match.ID = match.ID, ...)
)

names.SpatialPolygonsDataFrame = function(x) names(x@data)
"names<-.SpatialPolygonsDataFrame" = function(x,value) { checkNames(value); names(x@data) = value; x }

as.data.frame.SpatialPolygonsDataFrame = function(x, row.names, optional, ...) x@data

setAs("SpatialPolygonsDataFrame", "data.frame", function(from)
    as.data.frame.SpatialPolygonsDataFrame(from))

row.names.SpatialPolygonsDataFrame <- function(x) {
    .Call(SpatialPolygons_getIDs_c, x)
}

"row.names<-.SpatialPolygonsDataFrame" <- function(x, value) {
    spChFIDs(x, value)
}

setMethod("[", "SpatialPolygonsDataFrame", function(x, i, j, ... , drop = TRUE) {
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
        stop("matrix argument not supported in SpatialPolygonsDataFrame selection")
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
    if (any(is.na(i)))
		stop("NAs not permitted in row index")
	if (is.logical(i)) {
		if (length(i) == 1 && i)
			i = 1:length(x@polygons)
		else
			i <- which(i)
	} 
	if (is.character(i))
		i <- match(i, row.names(x))
    #SpatialPolygonsDataFrame(as(x, "SpatialPolygons")[i, , drop = FALSE],
    #    data = x@data[i, j, drop = FALSE], match.ID = FALSE)
	y <- new("SpatialPolygonsDataFrame")
	y@proj4string <- x@proj4string
	y@data = x@data[i, j, ..., drop = FALSE]

	y@polygons = x@polygons[i]
#	x@bbox <- .bboxCalcR(x@polygons)
	if (length(i) > 0) {
            y@bbox <- .Call(bboxCalcR_c, y@polygons)
            if (is.numeric(i) && i < 0) {
#                 area <- sapply(x@polygons, function(y) y@area)
#                 x@plotOrder <- as.integer(order(area, decreasing=TRUE))
                  y@plotOrder <- .Call(SpatialPolygons_plotOrder_c, y@polygons)
            } else {
	        y@plotOrder = order(match(i, x@plotOrder))
            }
	} else
	    y@bbox = x@bbox
	y
###
### RSB: do something with labelpoints here? How can I check they are present?
### (label points belong to the Polygons objects, not the SpatialPolygons object)
})

setAs("SpatialPolygonsDataFrame", "SpatialLinesDataFrame", 
	function(from) SpatialLinesDataFrame(as(from, "SpatialLines"),
		from@data, match.ID = FALSE))

dim.SpatialPolygonsDataFrame = function(x) dim(x@data)

setMethod("split", "SpatialPolygonsDataFrame", split.data.frame)

setMethod("geometry", "SpatialPolygonsDataFrame",
	function(obj) as(obj, "SpatialPolygons"))

length.SpatialPolygonsDataFrame = function(x) { length(x@polygons) }

# RSB 151030 override default coerce to preserve top-level comment
setAs("SpatialPolygonsDataFrame", "SpatialPolygons",
    function(from) {
        value <- new("SpatialPolygons")
        for (what in c("polygons", "plotOrder", "bbox", "proj4string"
            )) slot(value, what) <- slot(from, what)
        if (!is.null(comment(from))) comment(value) <- comment(from)
        value
    }
)
