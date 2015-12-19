chFIDsSpatialLines <- function(obj, x) {
    nl <- length(slot(obj, "lines"))
    if (length(x) != nl) stop("lengths differ")
    if (length(x) > length(unique(x))) stop("duplicate IDs")
#    for (i in 1:nl) slot(slot(obj, "lines")[[i]], "ID") <- x[i]
    lns <- slot(obj, "lines")
    lns1 <- vector(mode="list", length=nl)
    for (i in 1:nl) {
         lni <- lns[[i]]
         slot(lni, "ID") <- x[i]
         lns1[[i]] <- lni
     }
    slot(obj, "lines") <- lns1
    obj
}

setMethod("spChFIDs", signature(obj="SpatialLines", x="character"),
    chFIDsSpatialLines)

chFIDsSpatialLinesDataFrame <- function(obj, x) {
    SL <- as(obj, "SpatialLines")
    SLx <- spChFIDs(SL, x)
    df <- as(obj, "data.frame")
    row.names(df) <- sapply(slot(SLx, "lines"), function(x) slot(x, "ID"))
    SpatialLinesDataFrame(SLx, data=df)
}

setMethod("spChFIDs", signature(obj="SpatialLinesDataFrame", x="character"),
    chFIDsSpatialLinesDataFrame)

chFIDsSpatialPolygons <- function(obj, x) {
    np <- length(slot(obj, "polygons"))
    if (length(x) != np) stop("lengths differ")
    if (isTRUE(anyDuplicated(x))) stop("duplicate IDs")
#    for (i in 1:np) slot(slot(obj, "polygons")[[i]], "ID") <- x[i]
    pls <- slot(obj, "polygons")
    pls1 <- vector(mode="list", length=np)
    for (i in 1:np) {
         pli <- pls[[i]]
         slot(pli, "ID") <- x[i]
         pls1[[i]] <- pli
     }
    slot(obj, "polygons") <- pls1
    obj
}

setMethod("spChFIDs", signature(obj="SpatialPolygons", x="character"),
    chFIDsSpatialPolygons)

chFIDsSpatialPolygonsDataFrame <- function(obj, x) {
    SP <- as(obj, "SpatialPolygons")
    SPx <- spChFIDs(SP, x)
    df <- as(obj, "data.frame")
    row.names(df) <- .Call(SpatialPolygons_getIDs_c, SPx)
    SpatialPolygonsDataFrame(SPx, data=df)
}

setMethod("spChFIDs", signature(obj="SpatialPolygonsDataFrame", x="character"),
    chFIDsSpatialPolygonsDataFrame)
