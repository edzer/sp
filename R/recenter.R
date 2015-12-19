if (!isGeneric("recenter"))
	setGeneric("recenter", function(obj)
		standardGeneric("recenter"))

recenter.SpatialPolygons <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	projargs <- CRS(proj4string(obj))
	pls <- slot(obj, "polygons")
	Srl <- lapply(pls, recenter.Polygons)
	res <- SpatialPolygons(Srl, proj4string=projargs)
	res
}

setMethod("recenter", "SpatialPolygons", recenter.SpatialPolygons)

recenter.Polygons <- function(obj) {
	ID <- slot(obj, "ID")
	rings <- slot(obj, "Polygons")
	srl <- lapply(rings, recenter.Polygon)
	res <- Polygons(srl, ID=ID)
	res
}


recenter.Polygon <- function(obj) {
	crds <- slot(obj, "coords")
	hole <- slot(obj, "hole")
	inout <- (crds[,1] < 0)
	if (all(inout)) {
		crds[,1] <- crds[,1]+360
	} else {
		if (any(inout)) {
			crds[,1] <- ifelse(inout, crds[,1]+360, crds[,1])
		}
	}
	res <- Polygon(crds, hole)
	res
}



recenter.SpatialLines <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	projargs <- CRS(proj4string(obj))
	lns <- slot(obj, "lines")
	Sll <- lapply(lns, recenter.Lines)
	res <- SpatialLines(Sll, projargs)
	res
}

setMethod("recenter", "SpatialLines", recenter.SpatialLines)


recenter.Lines <- function(obj) {
	ID <- slot(obj, "ID")
	lines <- slot(obj, "Lines")
	sll <- lapply(lines, recenter.Line)
	res <- Lines(sll, ID=ID)
	res
}


recenter.Line <- function(obj) {
	crds <- coordinates(obj)
	inout <- (crds[,1] < 0)
	if (all(inout)) {
		crds[,1] <- crds[,1]+360
	} else {
		if (any(inout)) {
			crds[,1] <- ifelse(inout, crds[,1]+360, crds[,1])
		}
	}
	res <- Line(crds)
	res
}


