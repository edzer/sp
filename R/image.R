# first argument of image generic _needs_ to be x!
image.SpatialPixelsDataFrame = function(x, ...)
	image(as(x, "SpatialGridDataFrame"), ...)

image.SpatialPixels = function(x, ...) {
	x <- SpatialPixelsDataFrame(x, data=data.frame(rep(1,
		dim(coordinates(x))[1])))
	image(x, ...)
}

image.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2,
                col = heat.colors(12), 
		red=NULL, green=NULL, blue=NULL, axes = FALSE, xlim = NULL, 
		ylim = NULL, add = FALSE, ..., asp = NA, 
		setParUsrBB=FALSE, interpolate = FALSE, angle = 0,
                useRasterImage=(!.isSDI() && missing(breaks)), breaks, 
		zlim = range(as.numeric(x[[attr]])[is.finite(x[[attr]])])) {

	if (!add) 
		suppressWarnings(plot(as(x, "Spatial"),
			xlim = xlim, ylim = ylim, axes = axes, asp = asp, ..., 
			setParUsrBB=setParUsrBB))

	if (exists("rasterImage") && useRasterImage) {
		if (.isSDI()) 
			warning("Bug in SDI raster handling - your R graphics window may stop displaying output")
		bb <- bbox(x)
		scl <- function(xx, zlim) {
			xx = matrix(as.numeric(xx), nrow(xx), ncol(xx))
			#dr <- diff(range(x, na.rm = TRUE))
			dr = diff(zlim)
			#mx <- min(x, na.rm  = TRUE)
			mx = zlim[1]
			xx[xx < zlim[1] | xx > zlim[2]] = NA
			if (abs(dr) < .Machine$double.eps)
				res <- ifelse(is.na(xx), xx, 0.5)
			else res <- (xx - mx) / dr
				res
		}
	}
	if (is.null(red)) {
		if (exists("rasterImage") && useRasterImage) {
			if (!missing(breaks))
				warning("set useRasterImage to FALSE when using breaks")
			#x <- x[attr]
#                NAs <- is.na(x[[1]])
			m <-  scl(t(matrix(x[attr][[1]], x@grid@cells.dim[1],
                    x@grid@cells.dim[2])), zlim)
			m <- matrix(col[as.vector(m) * (length(col)-1) + 1], nrow(m), ncol(m))
			## if missing, set to transparent
#                m[is.na(m)] <- rgb(1, 1, 1, 0)
			rasterImage(m, bb[1,1], bb[2,1], bb[1,2], bb[2,2],
				interpolate = interpolate, angle = angle)
		} else {
			if (is.factor(x[[attr]]))
				x[[attr]] = as.numeric(x[[attr]])
			image(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), 
                  add = TRUE, col = col, zlim = zlim, breaks = breaks, ...)
		}
	} else {
	    if (is.null(green) || is.null(blue)) 
			stop("all colour bands must be given")
# modified to handle NAs in input (typical for coercion of Spatial Pixels
# to Spatial Grid)
		if (exists("rasterImage") && useRasterImage) {
	    	if (!missing(breaks))
				warning("set useRasterImage to FALSE when using breaks")
			xd <- x@data[, c(red, green, blue)]
			NAs <- is.na(xd[, 1]) | is.na(xd[, 2]) | is.na(xd[, 3])
			if (any(NAs))
				xd <- xd[!NAs, ]
			## create RGBs (using alpha=1 by default)
			RGBs <- rgb(xd, maxColorValue = 255)
			if (any(NAs)) {
				z <- rep(NA, length(NAs))
				z[!NAs] <- RGBs
				RGBs <- z
			}
			cv <- coordinatevalues(getGridTopology(x))
			m <- t(matrix(RGBs, x@grid@cells.dim[1], 
				x@grid@cells.dim[2], byrow = FALSE))
			rasterImage(m, bb[1,1], bb[2,1], bb[1,2], bb[2,2],
				interpolate = interpolate, angle = angle)
		} else {
			xd <- x@data[,c(red, green, blue)]
			NAs <- is.na(xd[,1]) | is.na(xd[,2]) | is.na(xd[,3])
			if (any(NAs)) xd <- xd[!NAs,]
			RGBs <- rgb(xd, maxColorValue = 255)
			if (any(NAs)) {
				z <- rep(NA, length(NAs))
				z[!NAs] <- RGBs
				RGBs <- z
			}
			fcols <- factor(RGBs)
			cv <- coordinatevalues(getGridTopology(x))
			m <- matrix(as.integer(fcols), x@grid@cells.dim[1], 
				x@grid@cells.dim[2], byrow=FALSE)
			res <- list(x=cv[[xcol]], y=sort(cv[[ycol]]), 
				z=m[,ncol(m):1,drop=FALSE])
			image(res, col=levels(fcols), add = TRUE, breaks = breaks, ...)
		}
	}
}

contour.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2,
                col = 1, add = FALSE, xlim = NULL, ylim = NULL,
                axes = FALSE, ..., setParUsrBB = FALSE)  {
	if (!add)
		plot(as(x, "Spatial"),
			xlim = xlim, ylim = ylim, axes = axes, ..., 
			setParUsrBB=setParUsrBB)
	contour(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), col = col,
          add = TRUE, ...)
}

contour.SpatialPixelsDataFrame = function(x, ...)
	contour(as(x, "SpatialGridDataFrame"), ...)

as.image.SpatialGridDataFrame = function(x, xcol = 1, ycol = 2, attr = 1) {
	cv = coordinatevalues(getGridTopology(x))
	m = as(x[attr], "matrix")
	list(x = cv[[xcol]], y = sort(cv[[ycol]]), z = m[,ncol(m):1,drop=FALSE])
}

# contributed by Michael Sumner 24 Oct 2007

image2Grid <- function (im, p4 = as.character(NA), digits=10) 
{
    if (!all(c("x", "y", "z") %in% names(im))) 
        stop("image must have components x, y, and z")
# RSB reversed test order
    lux <- length(unique(signif(diff(im$x), digits=digits)))
    luy <- length(unique(signif(diff(im$y), digits=digits)))
    if (lux > 1 || luy > 1) stop("x or y not equally spaced")
# RSB check for equal spacing
    cells.dim <- dim(im$z)
    xx <- im$x
    yy <- im$y
    lx <- length(xx)
    ly <- length(yy)
    if (all(c(lx, ly) == (cells.dim + 1))) {
        ##print("corners")
        if (!(lx == nrow(im$z) + 1 && ly == ncol(im$z) + 1 ) )
            stop("dimensions of z are not length(x)(-1) times length(y)(-1)")

        xx <- xx[-1] - diff(xx[1:2])/2
        yy <- yy[-1] - diff(yy[1:2])/2
    } else {

        if (!(lx == nrow(im$z) && ly == ncol(im$z)))
            stop("dimensions of z are not length(x) times length(y)")
    }

    SpatialGridDataFrame(GridTopology(c(xx[1], yy[1]), c(diff(xx[1:2]), 
        diff(yy[1:2])), cells.dim), data.frame(z = as.vector(im$z[, 
        ncol(im$z):1])), proj4string = CRS(p4))
}

# copied from the svMisc package, copyright Philippe Grosjean,
# Romain Francois & Kamil Barton
".isSDI" <- function()
{
	# This function is specific to Windows, but it is defined everywhere
	# so that we don't have to test the platform before use!
	# Check if Rgui was started in SDI mode (needed by some GUI clients)

	# 1) First is it Rgui?
	if (!.Platform$GUI[1] == "Rgui")
        return(FALSE)    # This is not Rgui

        # RGui SDI mode: returns "R Console", in MDI mode: returns "RGui"
        if (getIdentification() == "R Console") return(TRUE) else return(FALSE)

}

# menugget,
# from https://gist.github.com/menugget/7689145/raw/dac746aa322ca4160a5fe66c70fec16ebe26faf9/image.scale.2.r

#This function creates a color scale for use with the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "axis.pos" argument
#defines the side of the axis. The "add.axis" argument defines
#whether the axis is added (default: TRUE)or not (FALSE).
imageScale <- function(z, zlim, col = heat.colors(12),
     breaks, axis.pos = 1, add.axis = TRUE, ...) {
	if (!missing(breaks))
		if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
	if (missing(breaks) & !missing(zlim))
		breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
	if (missing(breaks) & missing(zlim)) {
		zlim <- range(z, na.rm=TRUE)
		zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
		zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
		breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
	}
	poly <- vector(mode="list", length(col))
	for (i in seq(poly))
		poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	if (axis.pos %in% c(1,3)){
		ylim<-c(0,1)
		xlim<-range(breaks)
	}
	if (axis.pos %in% c(2,4)) {
		ylim<-range(breaks)
		xlim<-c(0,1)
	}
	plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
	for (i in seq(poly)) {
		if(axis.pos %in% c(1,3))
			polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
		if(axis.pos %in% c(2,4))
			polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
	}
	box()
	if (add.axis) 
		axis(axis.pos)
}
