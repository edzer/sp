## 11/21/07 dhm
## version where shift works for lines and polygons
## adding option to rotate

if (!isGeneric("elide")) {
  setGeneric("elide", function(obj, ...) {
    standardGeneric("elide")
  })
}

elide.points <- function(obj, bb=NULL, shift=c(0, 0), reflect=c(FALSE, FALSE),
  scale=NULL, flip=FALSE, rotate=0, center=NULL, unitsq=FALSE) {
  if (length(shift) != 2L)
    stop("Two coordinate shift in input units required")
  if (!is.numeric(shift)) stop("shift not numeric")
  if (!is.logical(reflect)) stop("reflect must be logical")
  if (length(reflect) != 2L) stop("Two coordinate reflect required")
  if (!is.logical(flip)) stop("flip must be logical")
  if (!is.numeric(rotate)) stop("rotate not numeric")
  if (!is.null(center) && length(center) != 2L)
    stop("center must be numeric of length two")
  if (!is.logical(unitsq)) stop("unitsq must be logical")
  crds <- coordinates(obj)
  if (is.null(bb)) bb <- bbox(obj)
  if (rotate != 0 && is.null(center)) center <- bb[,1]
  if (rotate != 0) crds <- rotateCoords(crds, rotate, center)
  if (flip) {
    y <- crds[,1] + shift[1]
    x <- crds[,2] + shift[2]
    yr <- bb[1,] + shift[1]
    xr <- bb[2,] + shift[2]
  } else {
    x <- crds[,1] + shift[1]
    y <- crds[,2] + shift[2]
    xr <- bb[1,] + shift[1]
    yr <- bb[2,] + shift[2]
  }
  bb <- NULL
  if (!is.null(scale) && is.logical(scale) && scale && unitsq) {
    bb <- rbind(c(0,1), c(0,1))
    colnames(bb) <- c("min", "max")
  }
  scale <- scaleCoords(scale=scale, xr=xr, yr=yr)
  crds <- elideCoords(x=x, y=y, xr=xr, yr=yr, reflect=reflect, scale=scale)
  res <- SpatialPoints(crds, bbox=bb)
  res
}

elide.pointsdf <- function(obj, bb=NULL, shift=c(0, 0), 
  reflect=c(FALSE, FALSE), scale=NULL, flip=FALSE, rotate=0, center=NULL) {
  res <- elide(as(obj, "SpatialPoints"), bb=bb, shift=shift,
    reflect=reflect, scale=scale, flip=flip, rotate=rotate, center=center)
#    df <- as(obj, "data.frame")[,-c(1,2)]
#  df <- as(obj, "data.frame")
  df <- slot(obj, "data")
  res <- SpatialPointsDataFrame(res, data=df)
  res
}

setMethod("elide", signature(obj="SpatialPoints"), elide.points)

setMethod("elide", signature(obj="SpatialPointsDataFrame"), elide.pointsdf)

## rotate angle degrees clockwise around center
rotateCoords <- function(crds, angle=0, center= c(min(crds[,1]),min(crds[,2]))) {
  co <- cos(-angle*pi/180)
  si <- sin(-angle*pi/180)
  adj <- matrix(rep(center,nrow(crds)),ncol=2,byrow=TRUE)
  crds <- crds-adj
  cbind(co * crds[,1] - si * crds[,2],
        si * crds[,1] + co * crds[,2]) + adj
}

scaleCoords <- function(scale, xr, yr) {
  if (!is.null(scale)) {
    if (is.logical(scale) && scale) scale <- 1
    else if (!is.numeric(scale)) stop("scale neither TRUE nor numeric")
    dx <- abs(diff(xr))
    dy <- abs(diff(yr))
    md <- max(dx, dy)
    scale <- scale * (1/md)
  } else scale <- 1
  scale
}

elideCoords <- function(x, y, xr, yr, reflect, scale, rotate, center) {
  if (reflect[1]) {
    x <- xr[2] - x + xr[1]
  }
  if (reflect[2]) {
    y <- yr[2] - y + yr[1]
  }
  if (!isTRUE(all.equal(scale, 1))) {
    x <- (x - xr[1]) * scale
    y <- (y - yr[1]) * scale
  }
  crds <- cbind(x, y)
  crds
}

elide.lines <- function(obj, bb=NULL, shift=c(0, 0), reflect=c(FALSE, FALSE),
  scale=NULL, inverse=FALSE, flip=FALSE, rotate=0, center=NULL) {
  if (length(shift) != 2L)
    stop("Two coordinate shift in input units required")
  if (!is.numeric(shift)) stop("shift not numeric")
  if (!is.logical(reflect)) stop("reflect must be logical")
  if (length(reflect) != 2L) stop("Two coordinate reflect required")
  if (!is.logical(flip)) stop("flip must be logical")
  if (!is.numeric(rotate)) stop("rotate not numeric")
  if (!is.null(center) && length(center) != 2L)
    stop("center must be numeric of length two")
  if (is.null(bb)) bb <- bbox(obj)
  if (rotate != 0 && is.null(center)) center <- bb[,1]
  if (flip) {
    yr <- bb[1,] + shift[1]
    xr <- bb[2,] + shift[2]
  } else {
    xr <- bb[1,] + shift[1]
    yr <- bb[2,] + shift[2]
  }
  scale <- scaleCoords(scale=scale, xr=xr, yr=yr)
  lns <- slot(obj, "lines")
  new_lns <- lapply(lns, function(x) {
    Lns <- slot(x, "Lines")
    new_Lns <- lapply(Lns, function(y) {
      crds <- slot(y, "coords")
      ## rotate first, then elide (shift) [side effects if bb, scale or others supplied]
      if (rotate != 0) crds <- rotateCoords(crds, rotate, center)
      if (flip) {
        yc <- crds[,1] + shift[1]
        xc <- crds[,2] + shift[2]
      } else {
        xc <- crds[,1] + shift[1]
        yc <- crds[,2] + shift[2]
      }
      new_crds <- elideCoords(x=xc, y=yc, xr=xr, yr=yr, 
                              reflect=reflect, scale=scale)
      ## if want to elide first, then rotate:
      ## new_crds <- rotcrds(new_crds,rotate,center)
      Line(new_crds)})
    Lines(new_Lns, ID=slot(x, "ID"))})
  res <- SpatialLines(new_lns)
  res
}

elide.linesdf <- function(obj, bb=NULL, shift=c(0, 0), reflect=c(FALSE, FALSE),
  scale=NULL, inverse=FALSE, flip=FALSE, rotate=0, center=NULL) {
  res <- elide(as(obj, "SpatialLines"), bb=bb, shift=shift,
    reflect=reflect, scale=scale, flip=flip, rotate=rotate, center=center)
  df <- as(obj, "data.frame")
  res <- SpatialLinesDataFrame(res, data=df)
  res
}

setMethod("elide", signature(obj="SpatialLines"), elide.lines)

setMethod("elide", signature(obj="SpatialLinesDataFrame"), elide.linesdf)

elide.polygons <- function(obj, bb=NULL, shift=c(0, 0), reflect=c(FALSE, FALSE),
  scale=NULL, inverse=FALSE, flip=FALSE, rotate=0, center=NULL) {
  if (length(shift) != 2L)
    stop("Two coordinate shift in input units required")
  if (!is.numeric(shift)) stop("shift not numeric")
  if (!is.logical(reflect)) stop("reflect must be logical")
  if (length(reflect) != 2L) stop("Two coordinate reflect required")
  if (!is.logical(flip)) stop("flip must be logical")
  if (!is.numeric(rotate)) stop("rotate not numeric")
  if (!is.null(center) && length(center) != 2L)
    stop("center must be numeric of length two")
  if (is.null(bb)) bb <- bbox(obj)
  if (rotate != 0 && is.null(center)) center <- bb[,1]
  if (flip) {
    yr <- bb[1,] + shift[1]
    xr <- bb[2,] + shift[2]
  } else {
    xr <- bb[1,] + shift[1]
    yr <- bb[2,] + shift[2]
  }
  scale <- scaleCoords(scale=scale, xr=xr, yr=yr)
  pls <- slot(obj, "polygons")
  new_pls <- lapply(pls, function(x) {
    Pls <- slot(x, "Polygons")
    new_Pls <- lapply(Pls, function(y) {
      crds <- slot(y, "coords")
      if (rotate != 0) crds <- rotateCoords(crds,rotate,center)
      if (flip) {
        yc <- crds[,1] + shift[1]
        xc <- crds[,2] + shift[2]
      } else {
        xc <- crds[,1] + shift[1]
        yc <- crds[,2] + shift[2]
      }
      new_crds <- elideCoords(x=xc, y=yc, xr=xr, yr=yr, 
        reflect=reflect, scale=scale)
      Polygon(new_crds)})
    pres <- Polygons(new_Pls, ID=slot(x, "ID"))
    if (!is.null(comment(x))) comment(pres) <- comment(x)
    pres})
  res <- SpatialPolygons(new_pls)
  res
}

elide.polygonsdf <- function(obj, bb=NULL, shift=c(0, 0),
  reflect=c(FALSE, FALSE), scale=NULL, inverse=FALSE, flip=FALSE,
  rotate=0, center=NULL) {
  res <- elide(as(obj, "SpatialPolygons"), bb=bb, shift=shift,
    reflect=reflect, scale=scale, flip=flip, rotate=rotate, center=center)
  df <- as(obj, "data.frame")
  res <- SpatialPolygonsDataFrame(res, data=df)
  res
}

setMethod("elide", signature(obj="SpatialPolygons"), elide.polygons)

setMethod("elide", signature(obj="SpatialPolygonsDataFrame"), elide.polygonsdf)



