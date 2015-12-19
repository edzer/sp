# sp functions:
if (!isClass("ppp"))
	setOldClass("ppp")

if (!isClass("psp"))
	setOldClass("psp")

if (!isClass("owin"))
	setOldClass("owin")

if (!isClass("im"))
    setOldClass("im")

as.SpatialPoints.ppp =  function(from) {
    mult <- 1
    if (!is.null(from$window$units) && !is.null(from$window$units$multiplier))
        mult <- from$window$units$multiplier
    mx <- mult*from$x
    storage.mode(mx) <- "double"
    my <- mult*from$y
    storage.mode(my) <- "double"
    crds <- cbind(mx, my)
    if (from$window$type == "rectangle") {
        ow <- from$window
        bbox <- rbind(mult*as.double(ow$xrange), mult*as.double(ow$yrange))
        colnames(bbox) <- c("min", "max")
    } else bbox <- NULL
    SpatialPoints(coords=crds, bbox=bbox)
}
setAs("ppp", "SpatialPoints", as.SpatialPoints.ppp)

as.SpatialPointsDataFrame.ppp = function(from) {
	SP <- as(from, "SpatialPoints") 
	SpatialPointsDataFrame(SP, data.frame(marks = from$marks))
}
setAs("ppp", "SpatialPointsDataFrame", as.SpatialPointsDataFrame.ppp)

as.SpatialGridDataFrame.ppp = function(from) {
	w = from$window
	if (w$type != "mask")
		stop("window is not of type mask")
	offset = c(w$xrange[1] + 0.5 * w$xstep, w$yrange[1] + 0.5 * w$ystep)
	cellsize = c(diff(w$xrange)/w$dim[2], diff(w$yrange)/w$dim[1])
	dim = c(w$dim[2], w$dim[1])
	gt = GridTopology(offset, cellsize, dim)
	m = t(w$m[nrow(w$m):1,])
	m[!m] = NA
	data = data.frame(mask = as.vector(m))
	SpatialGridDataFrame(gt, data)
}
setAs("ppp", "SpatialGridDataFrame", as.SpatialGridDataFrame.ppp)

as.SpatialGridDataFrame.im = function(from) {
    offset = c(from$xrange[1] + 0.5 * from$xstep, from$yrange[1] + 
        0.5 * from$ystep)
    cellsize = c(diff(from$xrange)/from$dim[2], diff(from$yrange)/from$dim[1])
    dim = c(from$dim[2], from$dim[1])
    gt = GridTopology(offset, cellsize, dim)
    m = t(from$v[nrow(from$v):1,])
    data = data.frame(v = as.vector(m))
    SpatialGridDataFrame(gt, data)
}
setAs("im", "SpatialGridDataFrame", as.SpatialGridDataFrame.im)

#as.im.SpatialGridDataFrame = function(from) {
#    require(spatstat)
#    xi <- sp:::as.image.SpatialGridDataFrame(from)
#    im(t(xi$z), xcol=xi$x, yrow=xi$y)
#}
#setAs("SpatialGridDataFrame", "im", as.im.SpatialGridDataFrame)
