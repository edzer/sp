
# copied from raster:
if (!isGeneric("disaggregate")) {
    setGeneric("disaggregate", function(x, ...)
		standardGeneric("disaggregate"))
}

# Robert Hijmans:
explodePolygons <- function(x, ignoreholes=FALSE, ...) {
	npols <- length(x@polygons)
	crs <- x@proj4string
	count <- 0
	p <- NULL
	np <- vector(length=npols)
	for (i in 1:npols) {
		np[i] <- length(x@polygons[[i]]@Polygons)
		if (np[i] > 1) {
			parts <- x@polygons[[i]]@Polygons
			if (ignoreholes) {
				holes <- FALSE
			} else {
				holes <- sapply(parts, function(x)x@hole)
			}
			if (any(holes)) {
				if (np[i]==2) {
					pp <- x@polygons[[i]]
					pp@ID <- as.character(count + 1)
				} else {
					if (!requireNamespace("rgeos", quietly = TRUE))
						stop('package rgeos is needed to relate holes to their corresponding polygons')
					cmt <- as.integer(unlist(strsplit(rgeos::createPolygonsComment(x@polygons[[i]]), ' ')))
					cmt <- cbind(id=1:length(cmt), holeOf=cmt)
					cmt <- cmt[cmt[,2] > 0, ,drop=FALSE]
					pp <- NULL
					add <- 0
					for (j in unique(cmt[,2])) {
						# there might be multiple holes in a single polygon
						k <- cmt[cmt[,2]==j, 1]
						add <- add + 1
						pp <- c(pp, Polygons(x@polygons[[i]]@Polygons[c(j, k)], count + add))
					}
					x@polygons[[i]]@Polygons <- x@polygons[[i]]@Polygons[-unique(as.vector(cmt))]
					if (length(x@polygons[[i]]@Polygons) > 0) {
						parts <- x@polygons[[i]]@Polygons
						pp <- c(pp, sapply(1:length(parts), function(g) Polygons(parts[g], count + add + g)))
					}
				}
				np[i] <- np[i] - sum(holes)
			} else {
				pp <- sapply(1:np[i], function(g) Polygons(parts[g], count + g))
			}
		} else {
			pp <- x@polygons[[i]]
			pp@ID <- as.character(count + 1)
		}
		count <- count + np[i]
		p <- c(p, pp)
	}
	p <- SpatialPolygons(p)
	proj4string(p) <- crs
	
	if (.hasSlot(x, 'data')) {
		np <- rep(1:npols, np)
		x <- x@data[np, , drop = FALSE]
		#rownames(x) <- 1:nrow(x)
		rownames(x) <- NULL
		SpatialPolygonsDataFrame(p, x, FALSE)
	} else {
		p
	}
}



setMethod("disaggregate", "SpatialPolygons", 
	function(x,...) explodePolygons(x, ...))

setMethod("disaggregate", "SpatialPolygonsDataFrame", 
	function(x,...) explodePolygons(x, ...))

	
explodeLines <- function(x, ...) {
	nlins <- length(x@lines)
	crs <- x@proj4string
	count <- 0
	p <- NULL
	nl <- vector(length=nlins)
	for (i in 1:nlins) {
		parts <- x@lines[[i]]@Lines
		nl[i] <- length(parts)
		p <- c(p, sapply(1:nl[i], function(x) Lines(parts[x], count + x)))
		count <- count + nl[i]
	}
	p <- SpatialLines(p)
	proj4string(p) <- crs
	if (.hasSlot(x, 'data')) {
		nl <- rep(1:nlins, nl)
		x <- x@data[nl, , drop = FALSE]
		rownames(x) <- 1:nrow(x)
		SpatialLinesDataFrame(p, x, FALSE)
	} else
		p
}

setMethod("disaggregate", "SpatialLines", 
	function(x,...) explodeLines(x, ...))

setMethod("disaggregate", "SpatialLinesDataFrame", 
	function(x,...) explodeLines(x, ...))

# Roger, claims Barry wrote it first:
unfoldLines = function(x) {
	crds <- coordinates(x)
	nobjs <- sum(sapply(crds, length))
	out <- vector(mode="list", length=nobjs)
	i <- 1
	for (j in seq(along=crds)) {
  		jcrds <- crds[[j]]
  		for (k in seq(along=jcrds)) {
    		out[[i]] <- Lines(list(Line(jcrds[k])), as.character(i))
    		i <- i + 1
  		}
	}
	SLout <- SpatialLines(out)
	length(SLout) 
}
