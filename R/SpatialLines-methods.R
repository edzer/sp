Line <- function(coords) {
	coords <- coordinates(coords)
	if (ncol(coords) != 2)
		stop("coords must be a two-column matrix")
	new("Line", coords = coords)
}

Lines <- function(slinelist, ID) {
	if (is(slinelist, "Line"))
		slinelist = list(slinelist)
	if (any(sapply(slinelist, function(x) !is(x, "Line"))))
		stop("slinelist not a list of Line objects")
	if (missing(ID)) stop("Single ID required")
	if (length(ID) != 1) stop("Single ID required")
        ID <- as.character(ID)
        stopifnot(nzchar(ID))
	new("Lines", Lines = slinelist, ID=ID)
}

SpatialLines <- function(LinesList, proj4string=CRS(as.character(NA))) {
	if (any(sapply(LinesList, function(x) !is(x, "Lines"))))
		stop("lines list not exclusively filled with Lines objects")
	res <- new("SpatialLines", bbox = .bboxSls(LinesList), proj4string=proj4string, 
		lines=LinesList)
	res
}

LineLength = function(cc, longlat=FALSE, sum=TRUE) {
	if (is(cc, "Line"))
		cc = coordinates(cc)

	if (!is.matrix(cc)) stop("cc must be a matrix")
	if (ncol(cc) != 2) stop("cc must have two columns")
	if (!is.numeric(cc)) stop("cc must be numeric")
	x <- as.double(cc[,1])
	y <- as.double(cc[,2])
	n  <- as.integer(length(x))
	if (n == 1) return(0)
	lengths <- vector(mode="double", length=(n-1))
	lonlat <- as.integer(longlat)
	res <- .C("sp_lengths", x, y, n, lengths, lonlat, PACKAGE = "sp")[[4]]
	if (any(!is.finite(res))) stop("non-finite line lengths")
    if (sum) 
		sum(res)
	else
		res

#	dxy = matrix(apply(cc, 2, diff), ncol = 2)
#	sum(sqrt(apply(dxy, 1, function(x) sum(x ** 2))))
}

bbox.Lines <- function(obj) {
	rx=range(lapply(obj@Lines, function(x) range(x@coords[,1])))
	ry=range(lapply(obj@Lines, function(x) range(x@coords[,2])))
	res=rbind(x=rx,y=ry)
	dimnames(res)[[2]] <- c("min", "max")
	res
}

setMethod("bbox", "Lines", bbox.Lines)

bbox.Line <- function(obj) {
    rx <- range(obj@coords[,1])
    ry <- range(obj@coords[,2])
	res = rbind(x = rx, y = ry)
   	dimnames(res)[[2]] <- c("min", "max")
	res
}

setMethod("bbox", "Line", bbox.Line)

.bboxSls <- function(lst) {
	bb = sapply(lst, bbox)
	res = matrix(c(min(bb[1,]), min(bb[2,]), max(bb[3,]), max(bb[4,])), 2, 2)
	dimnames(res) = list(c("x", "y"), c("min", "max"))
	res
}


## plotSpatialLines <- function(SL, xlim = NULL, ylim = NULL,
## 	col = 1, lwd = 1, lty=1, add = FALSE, axes = FALSE, ...,
## 	setParUsrBB=FALSE)
## {
## #	frame()
## #	plot.window(xlim = xlim, ylim = ylim, asp = asp)
## 	if (! add)
## 		plot(as(SL, "Spatial"), xlim = xlim, ylim = ylim,
## 		    axes = axes, ..., setParUsrBB=setParUsrBB)
## 	lst <- SL@lines
## 	for (i in seq(along=lst)) {
## 		sllst = lst[[i]]@Lines
## 		for (j in seq(along=sllst)) {
## 			crds <- coordinates(sllst[[j]])
## 			if (length(col) != length(lst))
## 				# Michael Sumner, Jul 6 2010;
## 				# col <- rep(col[1], length(lst))
##  				col <- rep(col, length = length(lst))
## 			if (length(lwd) != length(lst))
## 				lwd <- rep(lwd[1], length(lst))
## 			if (length(lty) != length(lst))
## 				lty <- rep(lty[1], length(lst))
## 			lines(crds, col = col[i], lwd = lwd[i],
## 				lty = lty[i], ...)
## 		}
## 	}
## }

## MDS 2010-07-07 new handling for col, lwd, lty, and new lend, ljoin, lmitre
plotSpatialLines <- function(SL, xlim = NULL, ylim = NULL,
	col = 1, lwd = 1, lty=1, add = FALSE, axes = FALSE,
	lend = 0, ljoin = 0, lmitre = 10, ...,
	setParUsrBB=FALSE) {

	if (! add)
		plot(as(SL, "Spatial"), xlim = xlim, ylim = ylim,
		    axes = axes, ..., setParUsrBB=setParUsrBB)
	lst <- SL@lines

	if (length(col) != length(lst)) col <- rep(col, length = length(lst))
	if (length(lwd) != length(lst)) lwd <- rep(lwd, length = length(lst))
	if (length(lty) != length(lst)) lty <- rep(lty, length = length(lst))
	if (length(lend) != length(lst)) lend <- rep(lend, length = length(lst))
	if (length(ljoin) != length(lst)) ljoin <- rep(ljoin, length = length(lst))
	if (length(lmitre) != length(lst)) lmitre <- rep(lmitre, length = length(lst))

	for (i in seq(along=lst)) {
		sllst = lst[[i]]@Lines
		for (j in seq(along=sllst)) {
			crds <- coordinates(sllst[[j]])
			lines(crds, col = col[i], lwd = lwd[i], lty = lty[i],
				lend = lend[i], ljoin = ljoin[i], lmitre = lmitre[i])
		}
	}
}

setMethod("plot", signature(x = "SpatialLines", y = "missing"),
	function(x, y, ...) plotSpatialLines(x, ...))

setMethod("coordinates", "Line", function(obj) obj@coords)
setMethod("coordinates", "Lines", function(obj) lapply(obj@Lines, coordinates))
setMethod("coordinates", "SpatialLines", function(obj) lapply(obj@lines, coordinates))

lines.Line = function(x, y = NULL, ...) invisible(lines(coordinates(x), ...))
lines.Lines = function(x, y = NULL, ...) invisible(lapply(x@Lines,
	function(x, ...) lines(x, ...), ...))
lines.SpatialLines = function(x, y = NULL, ...) invisible(lapply(x@lines,
	function(x, ...) lines(x, ...), ...))

row.names.SpatialLines <- function(x) {
    sapply(slot(x, "lines"), slot, "ID")
}

"row.names<-.SpatialLines" <- function(x, value) {
	spChFIDs(x, value)
}

#"[.SpatialLines" =  function(x, i, j, ..., drop = T) {
setMethod("[", "SpatialLines", function(x, i, j, ..., drop = TRUE) {
	if (is(i, "Spatial"))
		i = !is.na(over(x, geometry(i)))
	if (is.logical(i)) {
		if (length(i) == 1 && i)
			i = 1:length(x@lines)
		else
			i <- which(i)
	} else if (is.character(i))
		i <- match(i, row.names(x))
	if (any(is.na(i))) 
		stop("NAs not permitted in row index")
	#SpatialLines(x@lines[i], CRS(proj4string(x)))
	x@lines = x@lines[i]
	if (length(x@lines) > 0)
		x@bbox = .bboxSls(x@lines)
	x
})

setMethod("coordnames", signature(x = "SpatialLines"),
	function(x) coordnames(x@lines[[1]])
)
setMethod("coordnames", signature(x = "Lines"),
	function(x) coordnames(x@Lines[[1]])
)
setMethod("coordnames", signature(x = "Line"),
	function(x) dimnames(coordinates(x))[[2]]
)
setReplaceMethod("coordnames",
	signature(x = "SpatialLines", value = "character"),
	function(x, value) {
		dimnames(x@bbox)[[1]] = value
		for (i in seq(along = x@lines))
			coordnames(x@lines[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames",
	signature(x = "Lines", value = "character"),
	function(x, value) {
		for (i in seq(along = x@Lines))
			coordnames(x@Lines[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames",
	signature(x = "Line", value = "character"),
	function(x, value) {
		dimnames(x@coords)[[2]] = value
		x
	}
)

getSpatialLinesMidPoints = function(SL) {
	ret = lapply(SL@lines,
		function(x) sapply(x@Lines,
			function(X) apply(X@coords, 2, mean)
		)
	)
	ret = t(sapply(ret, function(x) apply(x, 1, mean)))
	SpatialPoints(ret, CRS(proj4string(SL)))
}

LinesLength = function(Ls, longlat=FALSE) sum(sapply(Ls@Lines, LineLength, longlat))

SpatialLinesLengths = function(SL, longlat) {
        if (missing(longlat)) {
            proj <- is.projected(SL)
            if (is.na(proj)) {
                longlat <- FALSE
            } else {
                longlat <- !proj
            }
        }
        if (!is.logical(longlat)) stop("longlat should be logical")
	sapply(SL@lines, LinesLength, longlat=longlat)
}

setAs("Lines", "SpatialPoints", function(from) {
		cc = do.call(rbind, coordinates(from))
		if (!is.null(rownames(cc)))
			rownames(cc) = make.unique(rownames(cc))
		SpatialPoints(cc)
	}
)
setAs("SpatialLines", "SpatialPoints", function(from) {
		cc = do.call(rbind,
				lapply(from@lines, function(x) coordinates(as(x, "SpatialPoints"))))
		if (!is.null(rownames(cc)))
			rownames(cc) = make.unique(rownames(cc))
		SpatialPoints(cc, CRS(proj4string(from)))
	}
)
setAs("Lines", "SpatialMultiPoints", function(from) {
		SpatialMultiPoints(coordinates(from))
	}
)
setAs("SpatialLines", "SpatialMultiPoints", function(from) {
		l = lapply(from@lines, function(x) do.call(rbind, coordinates(x)))
		names(l) = sapply(from@lines, function(x) x@ID)
		SpatialMultiPoints(l, CRS(proj4string(from)))
	}
)
SpatialLines2SpatialPointsDataFrame = function(from) {
	spp = as(as(from, "SpatialLines"), "SpatialPoints")
	L = lapply(from@lines, function(x) {rep(1:length(x@Lines),
		times = sapply(x@Lines, function(x) nrow(x@coords)))})
	IDs = sapply(from@lines, function(x) x@ID)
	L2 = rep(IDs, times = sapply(L, length))
	L3 = rep(1:length(from@lines), times = sapply(L, length))
	L = unlist(L)
	SpatialPointsDataFrame(spp, data.frame(Lines.NR = L3, Lines.ID=L2, 
		Line.NR=L), proj4string=CRS(proj4string(from)))
}
setAs("SpatialLines", "SpatialPointsDataFrame", function(from)
	SpatialLines2SpatialPointsDataFrame(from)
)

setAs("SpatialPoints", "Line", function(from)
	Line(coordinates(from))
)

setAs("SpatialPoints", "Lines", function(from)
	Lines(as(from, "Line"), "ID")
)

setAs("SpatialPoints", "SpatialLines", function(from)
	SpatialLines(list(as(from, "Lines")), from@proj4string)
)

asWKTSpatialLines = function(x, digits = getOption("digits")) {
	ids = sapply(x@lines, function(x)slot(x,"ID"))
	df = data.frame(geometry = paste("MULTILINESTRING((",
		apply(
		signif(sapply(coordinates(x), function(x) x[[1]][1,]),digits=digits),
		2, paste, collapse=" ")," ...))",sep=""))
	row.names(df) = ids
	df
}
print.SpatialLines = function(x, ..., digits = getOption("digits"), 
		asWKT=FALSE) {
	cat("SpatialLines:\n")
	if (asWKT)
		print(asWKTSpatialLines(x, digits))
	else
		show(x)
	pst <- paste(strwrap(paste(
		"Coordinate Reference System (CRS) arguments:",
		proj4string(x))), collapse="\n")
	cat(pst, "\n")
}
#setMethod("show", "SpatialLines", function(object) print.SpatialLines(object))
length.SpatialLines = function(x) { length(x@lines) }

names.SpatialLines = function(x) { 
	unlist(lapply(x@lines, function(X) X@ID)) 
}

labels.SpatialLines = function(object, labelCRS, side = 1:2, ...) {
	# 1=below, 2=left, 3=above and 4=right.
	if (! identical(names(object), c("EW", "NS")))
		warning("this labels method is meant to operate on SpatialLines created with sp::gridlines")
	if (missing(labelCRS) && !is.na(proj4string(object)))
		labelCRS = object@proj4string

	cc = coordinates(object)
	pts = append(
		lapply(cc, function(x) do.call(rbind, lapply(x, function(y) head(y, 1)))),
		lapply(cc, function(x) do.call(rbind, lapply(x, function(y) tail(y, 1))))
	)
	d = SpatialPoints(do.call(rbind, lapply(pts, function(x) { row.names(x) = NULL; x})))
	d$pos = rep(c(2,1,4,3), times = rep(sapply(cc, length), 2))
	ang = append(
		lapply(cc, function(x) apply(do.call(rbind, lapply(x, function(y) apply(head(y, 2), 2, diff))), 1,
			function(x) atan2(x[2], x[1])*180/pi)),
		lapply(cc, function(x) apply(do.call(rbind, lapply(x, function(y) apply(tail(y, 2), 2, diff))), 1,
			function(x) atan2(x[2], x[1])*180/pi))
	)
	d$srt = c(ang[[1]], ang[[2]] - 90, ang[[3]], ang[[4]] - 90)

	# get the labels:
	if (! missing(labelCRS))
		object = spTransform(object, labelCRS) # may do nothing
	if (is.na(proj4string(object)))
		is.p = TRUE
	else 
		is.p = is.projected(object)

	pts = lapply(coordinates(object), function(x) do.call(rbind, lapply(x, function(y) y[1,])))
	lat = pts[[1]][,2]
	long = pts[[2]][,1]
	if (is.p)
		d$labels = as.character(rep(c(lat, long), 2))
	else
		d$labels = rep(c(degreeLabelsNS(lat), degreeLabelsEW(long)), 2)

	d[d$pos %in% side, ]
}
