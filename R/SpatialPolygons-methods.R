SpatialPolygons <- function(Srl, pO, proj4string=CRS(as.character(NA))) {
#	bb <- .bboxCalcR(Srl)
#	if (missing(pO)) {
#		area <- sapply(Srl, function(x) x@area)
#		pO <- as.integer(order(area, decreasing=TRUE))
#	}
#	Sp <- new("Spatial", bbox=bb, proj4string=proj4string)
#	res <- new("SpatialPolygons", Sp, polygons=Srl, plotOrder=as.integer(pO))
# RSB 091204
	if (missing(pO)) 
		pO <- NULL
	else {
		stopifnot(is.integer(pO))
		stopifnot(length(pO) == length(Srl))
	}
	stopifnot(is.list(Srl))
# tess to Polygons bug 121028
	# EJP, 2/3/2015, uncomments:
	# stopifnot(length(Srl) > 0)
	stopifnot(is(proj4string, "CRS"))
	res <- .Call(SpatialPolygons_c, Srl, pO, proj4string)
	validObject(res)
# 120416 add top-level comment to reduce comment checking
	cSr <- as.character(any(sapply(slot(res, "polygons"),
            function(x) !is.null(comment(x))), na.rm=TRUE))
	comment(res) <- cSr
	res
}

Polygon <- function(coords, hole=as.logical(NA)) {
	
	coords <- coordinates(coords)
##	if (!is.matrix(coords)) stop("coords must be a two-column matrix")
	if (ncol(coords) != 2) stop("coords must be a two-column matrix")
# RSB 091203
        n <- dim(coords)[1]
        stopifnot(is.logical(hole))
        ihole <- as.integer(hole)
# RSB 100126 fixing hole assumption
# thanks to Javier Munoz for report
        res <- .Call(Polygon_c, coords, n, ihole)
#        validObject(res)
        res
}

Polygons <- function(srl, ID) {
# tess to Polygons bug 121028
        stopifnot(is.list(srl))
        stopifnot(length(srl) > 0)
	if (any(sapply(srl, function(x) !is(x, "Polygon"))))
		stop("srl not a list of Polygon objects")
##	projargs <- unique(sapply(srl, proj4string))
##	if (length(projargs) > 1) 
##		stop("differing projections among Polygon objects")
	if (missing(ID)) stop("Single ID required")
	if (length(ID) != 1) stop("Single ID required")
        ID <- as.character(ID)
        stopifnot(nzchar(ID))
# RSB 091203
        res <- .Call(Polygons_c, srl, ID)
#        validObject(res)
        res
}

bbox.Polygons <- function(obj) {
	rx=range(c(sapply(obj@Polygons, function(x) range(x@coords[,1]))))
	ry=range(c(sapply(obj@Polygons, function(x) range(x@coords[,2]))))
	res=rbind(x=rx,y=ry)
   	colnames(res) <- c("min", "max")
	res
}

setMethod("bbox", "Polygons", bbox.Polygons)

bbox.Polygon <- function(obj) {
	rx <- range(obj@coords[,1])
   	ry <- range(obj@coords[,2])
	res=rbind(x=rx,y=ry)
    colnames(res) <- c("min", "max")
	res
}

setMethod("bbox", "Polygon", bbox.Polygon)

as.SpatialPolygons.PolygonsList <- function(Srl, proj4string=CRS(as.character(NA))) {
	if (any(sapply(Srl, function(x) !is(x, "Polygons"))))
		stop("srl not a list of Polygons objects")
#	projargs <- unique(sapply(Srl, proj4string))
#	if (length(projargs) > 1) 
#		stop("differing projections among Polygons objects")

#	n <- length(Srl)

	res <- SpatialPolygons(Srl, proj4string=proj4string)
	res
}

row.names.SpatialPolygons <- function(x) {
    .Call(SpatialPolygons_getIDs_c, x)
}

"row.names<-.SpatialPolygons" <- function(x, value) {
    spChFIDs(x, value)
}

setMethod("[", "SpatialPolygons", function(x, i, j, ..., drop = TRUE) {
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (is.logical(i)) {
		if (length(i) == 1 && i)
			i = 1:length(x@polygons)
		else
			i <- which(i)
	} else if (is.character(i))
		i <- match(i, row.names(x))
	if (any(is.na(i)))
		stop("NAs not permitted in row index")
	if (length(unique(i)) != length(i))
		stop("SpatialPolygons selection: can't find plot order if polygons are replicated")
	if (length(x@polygons[i]) == 0) {
		x@polygons = x@polygons[i]
		x@plotOrder = integer(0)
		stopifnot(validObject(x))
		x
	} else
		SpatialPolygons(x@polygons[i], proj4string=CRS(proj4string(x)))
#	x@polygons = x@polygons[i]
#	x@bbox <- .bboxCalcR(x@polygons)
#	area <- sapply(slot(x, "polygons"), function(i) slot(i, "area"))
#	x@plotOrder <- as.integer(order(area, decreasing=TRUE))
#	x
})

setMethod("coordnames", signature(x = "SpatialPolygons"), 
	function(x) coordnames(x@polygons[[1]])
)
setMethod("coordnames", signature(x = "Polygons"), 
	function(x) coordnames(x@Polygons[[1]])
)
setMethod("coordnames", signature(x = "Polygon"), 
	function(x) dimnames(x@coords)[[2]]
)
setReplaceMethod("coordnames", 
	signature(x = "SpatialPolygons", value = "character"),
	function(x, value) {
		dimnames(x@bbox)[[1]] = value
		#for (i in seq(along = x@polygons))
		#	coordnames(x@polygons[[i]]) = value
		x@polygons = lapply(x@polygons,
			function(y) Polygons(lapply(y@Polygons,
				function(z) { dimnames(z@coords)[[2]] = value; z }), y@ID))
		x
	}
)
setMethod("coordinates", "SpatialPolygons", 
	function(obj) {
		ret = t(sapply(slot(obj, "polygons"), function(i) slot(i, "labpt")))
		dimnames(ret) = list(sapply(slot(obj, "polygons"), function(i) slot(i, "ID")), NULL)
		ret
	}
)

getSpatialPolygonsLabelPoints = function(SP) {
	.Deprecated("slot", package = "sp",
            msg="use *apply and slot directly, or coordinates method")
	ret = t(sapply(slot(SP, "polygons"), function(x) slot(x, "labpt")))
	SpatialPoints(ret, CRS(proj4string(SP)))
}

as.Lines.Polygons = function(from) {
	lst = lapply(from@Polygons, function(x) as(x, "Line"))
	Lines(lst, from@ID)
}
setAs("Polygons", "Lines", as.Lines.Polygons)

as.SpatialLines.SpatialPolygons = function(from)
	SpatialLines(lapply(from@polygons, function(x) as(x, "Lines")),
		CRS(proj4string(from)))

setAs("SpatialPolygons", "SpatialLines", as.SpatialLines.SpatialPolygons)

as.SpatialPolygonsDataFrame.SpatialPolygons = function(from) {
	IDs <- sapply(slot(from, "polygons"), function(x) slot(x, "ID"))
	df <- data.frame(dummy = rep(0, length(IDs)), row.names=IDs)
	SpatialPolygonsDataFrame(from, df)
}

setAs("SpatialPolygons", "SpatialPolygonsDataFrame", 
	as.SpatialPolygonsDataFrame.SpatialPolygons)

length.SpatialPolygons = function(x) { length(x@polygons) }

names.SpatialPolygons = function(x) { 
	unlist(lapply(x@polygons, function(X) X@ID)) 
}
