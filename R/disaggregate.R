
# copied from raster:
if (!isGeneric("disaggregate")) {
    setGeneric("disaggregate", function(x, ...)
    standardGeneric("disaggregate"))
}

# Robert Hijmans:
explodePolygons <- function(x, ignoreholes=FALSE, ...) {
  warning("No rgeos support in sp from October 2023;\nsee https://r-spatial.org/r/2023/05/15/evolution4.html")
#  if (!requireNamespace("rgeos", quietly = TRUE))
    stop('use sf or terra functions')
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
          # cmt <- as.integer(unlist(strsplit(rgeos::createPolygonsComment(x@polygons[[i]]), ' ')))
          cmt <- comment(x@polygons[[i]])
		  if (is.null(cmt))
			stop("legacy Spatial object without polygon comments: please repair first using as(st_as_sf(x), \"Spatial\")")
          cmt <- as.integer(unlist(strsplit(cmt, ' ')))
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
  ps <- slot(p, "polygons")
  for (i in seq_along(ps)) {
    # comment(ps[[i]]) <- rgeos::createPolygonsComment(ps[[i]])
	n = length(ps[[i]]@Polygons)
    comment(ps[[i]]) <- do.call(paste, as.list(c("0", rep("1", n - 1)))) # they must be all holes now!
  }
  slot(p, "polygons") <- ps
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
  for (j in seq_along(crds)) {
      jcrds <- crds[[j]]
      for (k in seq_along(jcrds)) {
        out[[i]] <- Lines(list(Line(jcrds[k])), as.character(i))
        i <- i + 1
      }
  }
  SLout <- SpatialLines(out)
  length(SLout) 
}
