#
plot.SpatialPolygons <- function(x, col, border = par("fg"), add=FALSE, 
	xlim=NULL, ylim=NULL, xpd = NULL, density = NULL, angle = 45, 
	pbg=NULL, axes = FALSE, lty = par("lty"), ..., setParUsrBB=FALSE,
        usePolypath=NULL, rule=NULL, bgMap = NULL) {

	if (is.null(pbg))
		pbg = par("bg") # transparent!
	if (!is(x, "SpatialPolygons")) 
		stop("Not a SpatialPolygons object")
        if (is.null(usePolypath)) usePolypath <- get_Polypath()
        if (is.null(rule)) rule <- get_PolypathRule()

	if (! add) 
		plot(as(x, "Spatial"), xlim=xlim, ylim=ylim, axes = axes, 
			..., setParUsrBB=setParUsrBB, bgMap = bgMap)

	n <- length(slot(x, "polygons"))
	if (length(border) != n)
		border <- rep(border, n, n)
	polys <- slot(x, "polygons")
	pO <- slot(x, "plotOrder")
	if (!is.null(density)) {
		if (missing(col)) col <- par("fg")
		if (length(col) != n) col <- rep(col, n, n)
		if (length(density) != n)
			density <- rep(density, n, n)
		if (length(angle) != n)
			angle <- rep(angle, n, n)
		for (j in pO) 
			.polygonRingHoles(polys[[j]], border = border[j], 
			xpd = xpd, density = density[j], angle = angle[j], 
			col = col[j], pbg = pbg, lty=lty, ...) 
	} else {
		if (missing(col)) col <- NA
		if (length(col) != n) col <- rep(col, n, n)
		for (j in pO) 
			.polygonRingHoles(polys[[j]], col=col[j], 
			border=border[j], xpd = xpd, pbg = pbg, lty=lty, ...,
                        usePolypath=usePolypath, rule=rule)
	}
}

setMethod("plot", signature(x = "SpatialPolygons", y = "missing"),
	function(x, y, ...) plot.SpatialPolygons(x, ...))

.polygonRingHoles <- function(Sr, col=NA, border=NULL, xpd=NULL, density=NULL,
	angle=45, pbg, lty = par("lty"), ..., usePolypath=NULL,
        rule=NULL) {
	if (!is(Sr, "Polygons")) 
		stop("Not an Polygons object")
        if (is.null(usePolypath)) usePolypath <- get_Polypath()
        if (is.null(rule)) rule <- get_PolypathRule()
	if (!is.null(density)) hatch <- TRUE
	else hatch <- FALSE
	pO <- slot(Sr, "plotOrder")
	polys <- slot(Sr, "Polygons")

	if (hatch) {
	        for (i in pO) {
			if (!slot(polys[[i]], "hole"))
				.polygon(slot(polys[[i]], "coords"), 
					border = border, xpd = xpd, 
					density = density, angle = angle,
					col=col, hatch=TRUE, lty=lty, ...)
			else .polygon(slot(polys[[i]], "coords"), 
					border = border, xpd = xpd, col=pbg, 
					density = NULL, lty=lty, ...)
		} 
        } else if (exists("polypath") && usePolypath) {
            Srl <- as(Sr, "Lines")
            crds <- coordinates(Srl)
            if (length(crds) == 1) mcrds <- crds[[1]]
            else {
                NAr <- as.double(c(NA, NA))
                crds1 <- lapply(crds, function(x) rbind(x, NAr))
                mcrds <- do.call(rbind, crds1)
                mcrds <- mcrds[-nrow(mcrds),]
                rownames(mcrds) <- NULL
            }
            polypath(x=mcrds[,1], y=mcrds[,2], border=border, col=col,
                lty=lty, rule=rule, xpd=xpd, ...)
	} else {
	        for (i in pO) {
			if (!slot(polys[[i]], "hole"))
				.polygon(slot(polys[[i]], "coords"), 
					border = border, xpd = xpd, 
					col=col, lty=lty, ...)
			else .polygon(slot(polys[[i]], "coords"), 
				border = border, xpd = xpd, col=pbg, lty=lty,
                                ...)
		}
	}
}


.polygon = function(x, y = NULL, density = NULL, angle = 45,
	border = NULL, col = NA, lty = NULL, xpd = NULL, hatch=NA, ...) {
	if (is.na(hatch)) polygon(x = x, y = y, border = border, 
		col = col, lty = lty, xpd = xpd, ...)
	else polygon(x = x, y = y, density = density, angle = angle, 
		border = border, lty = lty, xpd = xpd, col=col, ...)
}


