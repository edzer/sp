Spatial <- function(bbox, proj4string = CRS(as.character(NA))) {
        new("Spatial", bbox=bbox, proj4string=proj4string)
}

if (!isGeneric("addAttrToGeom"))
	setGeneric("addAttrToGeom", function(x, y, match.ID, ...)
		standardGeneric("addAttrToGeom"))
if (!isGeneric("bbox"))
	setGeneric("bbox", function(obj)
		standardGeneric("bbox"))
if (!isGeneric("coordinates"))
	setGeneric("coordinates", function(obj, ...)
		standardGeneric("coordinates"))
if (!isGeneric("coordinates<-"))
	setGeneric("coordinates<-", function(object, value)
		standardGeneric("coordinates<-"))
if (!isGeneric("coordnames"))
	setGeneric("coordnames", function(x)
		standardGeneric("coordnames"))
if (!isGeneric("coordnames<-"))
	setGeneric("coordnames<-", function(x,value)
		standardGeneric("coordnames<-"))
if (!isGeneric("dimensions"))
	setGeneric("dimensions", function(obj)
		standardGeneric("dimensions"))
if (!isGeneric("fullgrid"))
	setGeneric("fullgrid", function(obj)
		standardGeneric("fullgrid"))
if (!isGeneric("fullgrid<-"))
	setGeneric("fullgrid<-", function(obj, value)
		standardGeneric("fullgrid<-"))
if (!isGeneric("geometry"))
	setGeneric("geometry", function(obj)
		standardGeneric("geometry"))
if (!isGeneric("geometry<-"))
	setGeneric("geometry<-", function(obj, value)
		standardGeneric("geometry<-"))
if (!isGeneric("gridded"))
	setGeneric("gridded", function(obj)
		standardGeneric("gridded"))
if (!isGeneric("gridded<-"))
	setGeneric("gridded<-", function(obj, value)
		standardGeneric("gridded<-"))
if (!isGeneric("is.projected"))
	setGeneric("is.projected", function(obj)
		standardGeneric("is.projected"))
#if (!isGeneric("overlay"))
#  	setGeneric("overlay", function(x, y, ...)
#  		standardGeneric("overlay"))
if (!isGeneric("over"))
	setGeneric("over", function(x, y, returnList = FALSE, fn = NULL, ...)
		standardGeneric("over"))
if (!isGeneric("plot"))
	setGeneric("plot", function(x, y, ...)
		standardGeneric("plot"))
if (!isGeneric("polygons"))
	setGeneric("polygons", function(obj)
		standardGeneric("polygons"))
if (!isGeneric("polygons<-"))
	setGeneric("polygons<-", function(object, value)
		standardGeneric("polygons<-"))
if (!isGeneric("proj4string"))
	setGeneric("proj4string", function(obj)
		standardGeneric("proj4string"))
if (!isGeneric("proj4string<-"))
	setGeneric("proj4string<-", function(obj, value)
		standardGeneric("proj4string<-"))
if (!isGeneric("sppanel"))
	setGeneric("sppanel", function(obj, ...)
		standardGeneric("sppanel"))
if (!isGeneric("spplot"))
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
if (!isGeneric("spsample"))
	setGeneric("spsample", function(x, n, type, ...)
		standardGeneric("spsample"))
if (!isGeneric("summary"))
	setGeneric("summary", function(object, ...)
		standardGeneric("summary"))
if (!isGeneric("spChFIDs"))
	setGeneric("spChFIDs", function(obj, x)
		standardGeneric("spChFIDs"))
if (!isGeneric("spChFIDs<-"))
	setGeneric("spChFIDs<-", function(obj, value)
		standardGeneric("spChFIDs<-"))
if (!isGeneric("surfaceArea"))
	setGeneric("surfaceArea", function(m, ...)
		standardGeneric("surfaceArea"))
if (!isGeneric("split"))
	setGeneric("split", function(x, f, drop = FALSE, ...)
		standardGeneric("split"))
if (!isGeneric("spTransform"))
	setGeneric("spTransform", function(x, CRSobj, ...)
		standardGeneric("spTransform"))
setMethod("spTransform", signature("Spatial", "CRS"), 
	function(x, CRSobj, ...) {
    	if (!requireNamespace("rgdal", quietly = TRUE))
			stop("package rgdal is required for spTransform methods")
		spTransform(x, CRSobj, ...) # calls the rgdal methods
	}
)
setMethod("spTransform", signature("Spatial", "character"), 
	function(x, CRSobj, ...) spTransform(x, CRS(CRSobj), ...)
)
setMethod("spTransform", signature("Spatial", "ANY"), 
	function(x, CRSobj, ...) stop("second argument needs to be of class CRS")
)


bbox.default <- function(obj) {
	is_points <- function(obj) {
	    is <- FALSE
	    if(is.array(obj))
		if(length(dim(obj))==2)
			if(dim(obj)[2]>=2) is <- TRUE
	    is
	}
	if(!is_points(obj))stop('object not a >= 2-column array')
	xr <- range(obj[,1],na.rm=TRUE)
	yr <- range(obj[,2],na.rm=TRUE)
	res <- rbind(x=xr, y=yr)
	colnames(res) <- c("min","max")
	res
}


setMethod("bbox", "ANY", bbox.default)

setMethod("bbox", "Spatial", function(obj) obj@bbox)

setMethod("dimensions", "Spatial", function(obj) nrow(bbox(obj)))

setMethod("polygons", "Spatial", function(obj) {
		if (is(obj, "SpatialPolygons"))
			as(obj, "SpatialPolygons")
		else
			stop("polygons method only available for objects of class or deriving from SpatialPolygons")
	}
)

summary.Spatial = function(object, ...) {
    obj = list()
	obj[["class"]] = class(object)
    obj[["bbox"]] = bbox(object)
    obj[["is.projected"]] = is.projected(object)
    obj[["proj4string"]] = object@proj4string@projargs
    if (is(object, "SpatialPoints"))
        obj[["npoints"]] = nrow(object@coords)
	if (is(object, "SpatialGrid") || is(object, "SpatialPixels"))
		obj[["grid"]] = gridparameters(object)
	if ("data" %in% slotNames(object) && ncol(object@data) > 0)
		obj[["data"]] = summary(object@data)
    class(obj) = "summary.Spatial"
    obj
}
setMethod("summary", "Spatial", summary.Spatial)

print.summary.Spatial = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
    cat("Coordinates:\n")
    print(x[["bbox"]], ...)
    cat(paste("Is projected:", x[["is.projected"]], "\n"))
#    cat(paste("proj4string : [", x[["proj4string"]], "]\n", sep=""))
    pst <- paste(strwrap(x[["proj4string"]]), collapse="\n")
    if (nchar(pst) < 40) cat(paste("proj4string : [", pst, "]\n", sep=""))
    else cat(paste("proj4string :\n[", pst, "]\n", sep=""))
    if (!is.null(x$npoints)) {
        cat("Number of points: ")
        cat(x$npoints)
        cat("\n")
    }
    if (!is.null(x$n.polygons)) {
        cat("Number of polygons: ")
        cat(x$n.polygons)
        cat("\n")
    }
	if (!is.null(x$grid)) {
        cat("Grid attributes:\n")
        print(x$grid, ...)
    }
    if (!is.null(x$data)) {
        cat("Data attributes:\n")
        print(x$data, ...)
    }
    invisible(x)
}

# sp.axes = FALSE

#asp <- function(x, ylim) {
#	if (is.na(proj4string(x)) || is.projected(x))
#		return(1.0)
#	else
#		return(1/cos((mean(ylim) * pi)/180))
#}

bb2merc = function(x, cls = "ggmap") { # return bbox in the appropriate "web mercator" CRS
	WGS84 = CRS("+init=epsg:4326")
	# merc = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
	merc = CRS("+init=epsg:3857") # http://wiki.openstreetmap.org/wiki/EPSG:3857
	if (cls == "ggmap") {
		b = sapply(attr(x, "bb"), c)
		pts = cbind(c(b[2],b[4]),c(b[1],b[3]))
	} else if (cls == "RgoogleMaps")
		pts = rbind(x$BBOX$ll, x$BBOX$ur)[,2:1]
	else
		stop("unknown cls")
	bbox(spTransform(SpatialPoints(pts, WGS84), merc))
}

plot.Spatial <- function(x, xlim = NULL, ylim = NULL, 
	asp = NA, axes = FALSE, bg = par("bg"), ..., 
	xaxs, yaxs, lab, setParUsrBB = FALSE, bgMap = NULL, expandBB = c(0,0,0,0)) {

	# expandBB: 1=below, 2=left, 3=above and 4=right.
	bbox <- bbox(x)
	expBB = function(lim, expand) c(lim[1] - expand[1] * diff(lim), lim[2] + expand[2] * diff(lim))
	if (is.null(xlim)) 
		xlim <- expBB(bbox[1,], expandBB[c(2,4)])
	if (is.null(ylim)) 
		ylim <- expBB(bbox[2,], expandBB[c(1,3)])
	if (is.na(asp)) 
		asp <- ifelse(is.na(proj4string(x)) || is.projected(x), 1.0, 
			1/cos((mean(ylim) * pi)/180))

	plot.new()

	args = list(xlim = xlim, ylim = ylim, asp = asp)
	if (!missing(xaxs)) args$xaxs = xaxs
	if (!missing(yaxs)) args$yaxs = yaxs
	if (!missing(lab)) args$lab = lab
	do.call(plot.window, args)

	if (setParUsrBB) 
		par(usr=c(xlim, ylim))
	pl_reg <- par("usr")
	rect(xleft=pl_reg[1], ybottom=pl_reg[3], xright=pl_reg[2], 
		ytop=pl_reg[4], col=bg, border=FALSE)
	if (axes) { # set up default axes system & box:
		box()
		if (identical(is.projected(x), FALSE)) {
			degAxis(1, ...)
			degAxis(2, ...)
		} else {
			axis(1, ...)
			axis(2, ...)
		}
#		axis(3, labels = FALSE, ...)
#		axis(4, labels = FALSE, ...)
	}
	localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
	localTitle(...)
	if (!is.null(bgMap)) {
		is3875 = function(x) length(grep("+init=epsg:3857", x@proj4string@projargs)) > 0
		mercator = FALSE
		if (is(bgMap, "ggmap")) {
			bb = bb2merc(bgMap, "ggmap")
			mercator = TRUE
		} else if (all(c("lat.center","lon.center","zoom","myTile","BBOX") %in% names(bgMap))) {
			# an object returned by RgoogleMaps::GetMap
			bb = bb2merc(bgMap, "RgoogleMaps")
			bgMap = bgMap$myTile
			mercator = TRUE
		} else
			bb = rbind(xlim, ylim) # can be any CRS!
		if (mercator && !is3875(x))
			warning(paste('CRS of plotting object differs from that of bgMap, which is assumed to be CRS("+init=epsg:3857")'))
		rasterImage(bgMap, bb[1,1], bb[2,1], bb[1,2], bb[2,2], interpolate = FALSE)
	}
}
setMethod("plot", signature(x = "Spatial", y = "missing"), 
	function(x,y,...) plot.Spatial(x,...))

degAxis = function (side, at, labels, ...) {
		if (missing(at))
        	at = axTicks(side)
        if (missing(labels)) {
			labels = FALSE
        	if (side == 1 || side == 3)
               	labels = parse(text = degreeLabelsEW(at))
        	else if (side == 2 || side == 4)
               	labels = parse(text = degreeLabelsNS(at))
		} 
        axis(side, at = at, labels = labels, ...)
}

setReplaceMethod("spChFIDs", signature(obj = "Spatial", value = "ANY"),
	function(obj, value) { spChFIDs(obj, as.character(value)) }
)

setReplaceMethod("coordinates", signature(object = "Spatial", value = "ANY"),
	function(object, value) 
		stop("setting coordinates cannot be done on Spatial objects, where they have already been set")
)

setMethod("[[", c("Spatial", "ANY", "missing"), 
	function(x, i, j, ...) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		x@data[[i]]
	}
)

setReplaceMethod("[[", c("Spatial", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		if (is.character(i) && any(!is.na(match(i, dimnames(coordinates(x))[[2]]))))
			stop(paste(i, "is already present as a coordinate name!"))
		x@data[[i]] <- value
		x
	}
)

setMethod("$", "Spatial", 
	function(x, name) {
		if (!("data" %in% slotNames(x)))
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)

setReplaceMethod("$", "Spatial", 
	function(x, name, value) { 
		if (name %in% coordnames(x))
			stop(paste(name, 
				"is a coordinate name, please choose another name"))
		if (!("data" %in% slotNames(x))) {
			df = list(value); names(df) = name
			return(addAttrToGeom(x, data.frame(df), match.ID = FALSE))
			# stop("no $<- method for object without attributes")
		}
		#if (is.list(value))
		#	warning("assigning list or data.frame to attribute vector")
		x@data[[name]] = value 
		x 
	}
)

setMethod("geometry", "Spatial",
	function(obj) { 
		if ("data" %in% slotNames(obj))
			stop(paste("geometry method missing for class", class(obj)))
		obj 
	}
)

setReplaceMethod("geometry", c("data.frame", "Spatial"),
	function(obj, value) addAttrToGeom(value, obj)
)

setReplaceMethod("[", c("Spatial", "ANY", "ANY", "ANY"),
	function(x, i, j, value) {
		if (!("data" %in% slotNames(x)))
			stop("no [ method for object without attributes")
		if (is.character(i) && any(!is.na(match(i, dimnames(coordinates(x))[[2]]))))
			stop(paste(i, "is already present as a coordinate name!"))
		x@data[i,j] <- value
		x
	}
)

# Don MacQueen provided head & tail:
head.Spatial <- function(x, n=6L, ...) {
    ix <- sign(n)*seq(abs(n))
    x[ ix , , drop=FALSE]
}

tail.Spatial <- function(x, n=6L, ...) {
    ix <- sign(n)*rev(seq(nrow(x), by=-1L, len=abs(n)))
    x[ ix , , drop=FALSE]
}
