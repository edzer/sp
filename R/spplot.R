setMethod("sppanel", "SpatialPolygons",
  function(obj, col = 1, fill = "transparent", ...) {
	if (is.character(obj))
		obj = get(obj)
	if (!is(obj, "SpatialPolygons"))
		stop(paste(
		"object extending class SpatialPolygons expected; got class",
		class(obj)))
	else
		obj = as(obj, "SpatialPolygons")
	if (get_Polypath()) {
		lo = length(obj)
		obj = as(as(obj, "SpatialLines"), "SpatialPointsDataFrame")
		cc = coordinates(obj)
		#id = as.numeric(obj$Line.NR)
		id = as.numeric(obj$Lines.NR * max(obj$Line.NR) + (obj$Line.NR - 1))
		if (length(fill) > 1 || length(col) > 1) {
			fill = rep(fill, length.out = lo)
			col = rep(col, length.out = lo)
			for (i in 1:lo) {
				sel = obj$Lines.NR == i
				grid.path(cc[sel,1], cc[sel,2], id[sel], 
					default.units = "native", 
					gp = gpar(col = col[i], fill = fill[i], ...))
			}
		} else 
			grid.path(cc[,1], cc[,2], id, default.units = "native",
				gp = gpar(col = col, fill = fill, ...))
	} else {
		sp.polygon3 = function(x, col, fill, ...) { 
			cc = slot(x, "coords")
			grid.polygon(cc[,1], cc[,2], default.units = "native", 
				gp = gpar(col = col, fill = fill, ...))
			panel.lines(cc, col = col, ...)
		}
		pls = slot(obj, "polygons")
   		pO <- slot(obj, "plotOrder")
		if (length(fill) != length(pO)) 
			fill <- rep(fill[1], length(pO))
		for (i in pO) {
			Srs <- slot(pls[[i]], "Polygons")
			pOi <- slot(pls[[i]], "plotOrder")
			for (j in pOi)
				sp.polygon3(Srs[[j]], col = col, fill = fill[i], ...)
		}
	}
})

# backward compatibility:
sp.polygons = function(obj, col = 1, fill = "transparent",...) 
	sppanel(obj, col = col, fill = fill, ...)

setMethod("sppanel", "SpatialLines", 
  function (obj, col = 1, ...) {
    ## contributed by Josh O'Brien, Mar 15, 2015
	lo <- length(obj@lines)
	col <- rep(col, length.out = lo)
	lapply(seq_len(lo), function(ii) sppanel(obj@lines[[ii]], col = col[ii], ...))
})

setMethod("sppanel", "Lines",
  function (obj, col = 1, ...) lapply(obj@Lines, sppanel, col = col, ...))

setMethod("sppanel", "Line",
  function (obj, col = 1, ...) panel.lines(coordinates(obj), col = col, ...))

# backward compatibility:
sp.lines = function(obj, col = 1,...) sppanel(obj, col = col,...)

setMethod("sppanel", "SpatialPoints",
	function(obj, pch = 3, ...)
		panel.points(coordinates(obj), pch = pch, ...))
# backward compatibility:
sp.points = function(obj, pch=3, ...) sppanel(obj, pch = pch, ...)

sp.grid = function(obj, col = 1, alpha = 1, ..., at = pretty(obj[[1]]),
		col.regions = col) {
	xy = coordinates(obj)
	if (length(col) > 1 && ("data" %in% slotNames(obj))) {
		z = obj[[1]]
		if (is.factor(z))
			col = col[z]
		else  # cut:
    		col = level.colors(z, at, col.regions, colors = TRUE)
	}
	gt = as(getGridTopology(obj), "data.frame")
	grid.rect(x = xy[,1], y = xy[,2], width = gt$cellsize[1],
		height = gt$cellsize[2], default.units = "native",
		gp = gpar(fill = col, col = NA, alpha = alpha))
}
setMethod("sppanel", "SpatialPixels", sp.grid)
setMethod("sppanel", "SpatialGrid", sp.grid)

sp.text = function(loc, txt, ...) {
	if (!is.numeric(loc))
		stop("loc (first argument) should be numeric, indicating text locations")
	if (length(loc) == 2)
		panel.text(loc[1], loc[2], txt, ...)
	else if (is.matrix(loc) && ncol(loc) == 2 && nrow(loc) == length(txt))
		panel.text(loc[,1], loc[,2], txt, ...)
	else
		stop("loc and txt have non-matching dimensions")
}
setMethod("sppanel", "character", function(obj,txt, ...) sp.text(obj, txt, ...))

sp.panel.layout = function(lst, p.number, ...) { # now obsolete...
	.Deprecated("sppanel")
	sp.panel0 = function(x, first = FALSE, ...) {
		if (inherits(x, "list")) {
			if (!is.null(x$which) && is.na(match(p.number, x$which)))
				return()
			# print(paste(class(x), "first val", first, "first obj", x$first))
			if (!is.null(x$first)) {
				if (x$first == first)
					do.call(x[[1]], x[2:length(x)])
			} else if (!first)
				do.call(x[[1]], x[2:length(x)])
		} 
	}
	if (!is.null(lst$which) && is.na(match(p.number, lst$which)))
		return()
	else
		lst$which = NULL
	if (is.null(lst))
		return()
	if (inherits(lst, "list")) {
		if (inherits(lst[[1]], "list")) 
			lapply(lst, sp.panel0, ...)
		else
			sp.panel0(lst, ...)
	} else
		stop(paste("expected object of class list; got object of class", class(lst)))
}

setMethod("sppanel", "NULL", function(obj,...) { })

sppanelList = function(obj, p.number, first, ...) {
	missingFirst = missing(first)
	if (length(obj) == 1 & is.null(obj[[1]]))
		return()
	if (!is.null(obj$which) && is.na(match(p.number, obj$which)))
		return() # false panel
	else 
		obj$which = NULL # continue: either all panels, or right panel
	if (is.list(obj[[1]])) # list-of-lists, recurse:
		return(lapply(obj, sppanel, p.number = p.number, first = first, ...))
	opaque = function(x) (is(x, "SpatialPolygons") || is(x, "SpatialGrid") || is(x, "SpatialPixels"))
	if (is.character(obj[[1]]) || is.function(obj[[1]])) {
		if (is.null(obj$first))
			obj$first = opaque(obj[[2]]) # default: grids/polygons behind, rest front
		if (missingFirst) # meaning: do plot it
			first = obj$first
		if (obj$first == first) {
			obj$first = NULL
			do.call(obj[[1]], obj[-1], ...)
		}
	} else {
		sp = sapply(obj, is, "Spatial")
		stopifnot(any(sp))
		lapply(obj[sp], function(x) { 
			if (missingFirst || identical(obj$first, first) || opaque(x) == first)
				do.call(sppanel, append(x, obj[!sp]), ...)
		})
	}
}
setMethod("sppanel", "list", sppanelList)

getFormulaLevelplot = function(sdf, zcol) {
	if (length(zcol) > 1)
		as.formula(paste("z~", paste(dimnames(coordinates(sdf))[[2]], 
			collapse = "+"), "|name"))
	else {
		if (!is.character(zcol)) 
			zcol = names(sdf)[zcol]
		as.formula(paste(zcol, "~", paste(dimnames(coordinates(sdf))[[2]],
			collapse = "+")))
	}
}

spplot.grid = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
		aspect = mapasp(obj,xlim,ylim), panel = panel.gridplot, sp.layout = NULL, formula, 
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,], checkEmptyRC = TRUE,
		col.regions = get_col_regions()) {
	if (is.null(zcol)) stop("no names method for object")
	if (checkEmptyRC)
		sdf = addNAemptyRowsCols(obj) # returns SpatialPointsDataFrame
	else
		sdf = as(obj, "SpatialPointsDataFrame")
	if (missing(formula))
		formula = getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1) {
		sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		zcol2 = "z"
	} else
		zcol2 = zcol
    if (exists("panel.levelplot.raster")) {
        opan <- lattice.options("panel.levelplot")[[1]]
        lattice.options("panel.levelplot"="panel.levelplot.raster")
#       cat("using raster panel\n")
    }
	scales = longlat.scales(obj, scales, xlim, ylim)
	args = append(list(formula, data = as(sdf, "data.frame"), 
		aspect = aspect, panel = panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, xlim = xlim, ylim = ylim, col.regions = col.regions), 
		list(...))
	# deal with factor variables:
	if (all(unlist(lapply(obj@data[zcol], is.factor)))) {
		#if (!is.null(args$col.regions) &&
		#		nlevels(obj@data[[zcol[1]]]) != length(args$col.regions))
		#	stop("length of col.regions should match number of factor levels")
		args$data[[zcol2]] = as.numeric(args$data[[zcol2]])
		if (is.null(args$colorkey) || (is.logical(args$colorkey) && args$colorkey)
				|| (is.list(args$colorkey) && is.null(args$colorkey$at) && 
					is.null(args$colorkey$labels))) {
			if (!is.list(args$colorkey))
				args$colorkey = list()
			ck = args$colorkey
			args$colorkey = NULL
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck))
		} else
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck, FALSE))
	}
	ret = do.call(levelplot, args)
    if (exists("panel.levelplot.raster"))
        lattice.options("panel.levelplot" = opan)
	ret 
}

setMethod("spplot", signature("SpatialPixelsDataFrame"), spplot.grid)
setMethod("spplot", signature("SpatialGridDataFrame"), 
	function(obj, ...) spplot.grid(as(obj, "SpatialPixelsDataFrame"), ...))

spplot.polygons = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
		aspect = mapasp(obj,xlim,ylim), 
		panel = panel.polygonsplot, sp.layout = NULL, formula, 
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,],
		col.regions = get_col_regions()) {

	if (is.null(zcol)) stop("no names method for object")
	sdf = as(obj, "data.frame")
	if (is(obj, "SpatialPolygonsDataFrame"))
		labpts = coordinates(obj)
	else {
		# get first points of each lines object:
		n = length(obj@lines)
		labpts = matrix(unlist(lapply(obj@lines, function(x) 
			lapply(x@Lines[1], function(x) coordinates(x)[1,]))), 
				n, 2, byrow=TRUE) 
	}
	dimnames(labpts)[[2]] = c("xlabelpoint", "ylabelpoint")
	sdf = as.data.frame(cbind(labpts, sdf))
	coordinates(sdf) = c("xlabelpoint", "ylabelpoint")
	if (missing(formula))
		formula = getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1) {
		sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		zcol2 = "z"
	} else
		zcol2 = zcol
	if (is(obj, "SpatialPolygonsDataFrame"))
		grid.polygons = as(obj, "SpatialPolygons")
	else
		grid.polygons = as(obj, "SpatialLines")
	scales = longlat.scales(obj, scales, xlim, ylim)

	args = append(list(formula, data = as(sdf, "data.frame"),
		aspect = aspect, grid.polygons = grid.polygons, panel =
		panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, xlim = xlim, ylim = ylim,
		col.regions = col.regions), list(...))
	if (all(unlist(lapply(obj@data[zcol], is.factor)))) {
		#if (!is.null(args$col.regions) &&
		#		nlevels(obj@data[[zcol[1]]]) != length(args$col.regions))
		#	stop("length of col.regions should match number of factor levels")
		args$data[[zcol2]] = as.numeric(args$data[[zcol2]])
		if (is.null(args$colorkey) 
				|| (is.logical(args$colorkey) && args$colorkey)
				|| (is.list(args$colorkey) && is.null(args$colorkey$at) && 
					is.null(args$colorkey$labels))) {
			if (!is.list(args$colorkey))
				args$colorkey = list()
			ck = args$colorkey
			args$colorkey = NULL
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck))
		} else
			args = append(args, colorkey.factor(obj[[zcol[1]]], ck, FALSE))
	}
	do.call(levelplot, args)
}

setMethod("spplot", signature("SpatialPolygonsDataFrame"), spplot.polygons)
setMethod("spplot", signature("SpatialLinesDataFrame"), spplot.polygons)

spplot.points = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
		aspect = mapasp(obj,xlim,ylim), panel = panel.pointsplot,
		sp.layout = NULL, identify = FALSE, formula,
		xlim = bbexpand(bbox(obj)[1,], 0.04), 
		ylim = bbexpand(bbox(obj)[2,], 0.04),
		edge.col = "transparent", colorkey = FALSE,
		col.regions = get_col_regions()) 
{

	if (is.null(zcol)) stop("no names method for object")
	dots = list(...)
	sdf = obj
	if (!is.character(zcol)) 
		zcol = names(sdf)[zcol]
	# create formula:
	if (missing(formula)) {
		if (length(zcol) > 1) {
			formula = as.formula(paste(paste(dimnames(coordinates(sdf))[[2]][2:1], 
				collapse = "~"), "|name"))
			sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		} else {
			if (!is.character(zcol)) 
				zcol = names(sdf)[zcol]
			ccn = dimnames(coordinates(sdf))[[2]]
			formula = as.formula(paste(ccn[2], "~", ccn[1]))
		}
	}
	scales = longlat.scales(obj, scales, xlim, ylim)
	args.xyplot = append(list(formula, data = as(sdf, "data.frame"), 
		panel = panel, aspect = aspect, scales = scales, 
		xlab = xlab, ylab = ylab, sp.layout = sp.layout,
		xlim = xlim, ylim = ylim, edge.col = edge.col,
		col.regions = col.regions), dots)
	z = create.z(as(obj, "data.frame"), zcol)
	args.xyplot = fill.call.groups(args.xyplot, z = z, edge.col = edge.col, 
		colorkey = colorkey, ...)
	# debug:
	#print(args.xyplot)
	plt = do.call(xyplot, args.xyplot)
	if (!(is.logical(identify) && identify==FALSE) && interactive()) {
		print(plt)
		if (!(is.numeric(identify) && length(identify) == 2))
			identify = c(1,1)
		trellis.focus("panel", identify[1], identify[2])
		labels = row.names(as(sdf, "data.frame"))
		cat("left-mouse to identify points; right-mouse to end\n")
		cc = coordinates(obj)
		ret = panel.identify(cc[,1], cc[,2], labels)
		trellis.unfocus()
		return(ret)
	} else
		plt
}
setMethod("spplot", signature("SpatialPointsDataFrame"), spplot.points)

setMethod("spplot", signature("SpatialMultiPointsDataFrame"), 
	function(obj, ...) spplot.points(as(obj, "SpatialPointsDataFrame"), ...))

create.z = function(df, zcol) {
	if (is.logical(df[[zcol[1]]])) {
		z = stack(df[zcol])[[1]]
		z = as.factor(z)
	} else if (is.numeric(df[[zcol[1]]]))
		z = stack(df[zcol])[[1]]
	else if (is.factor(df[[zcol[1]]])) {
		lev = levels(df[[zcol[1]]])
		z = factor(as.vector(sapply(df[zcol], as.character)), levels = lev)
	} else
		stop("no support for variable of this type")
	z
}

panel.gridplot = function(x, y, z, subscripts, ..., sp.layout) {
	sppanel(list(sp.layout), panel.number(), first = TRUE)
	panel.levelplot(x, y, z, subscripts, ...)
	sppanel(list(sp.layout), panel.number(), first = FALSE)
}

panel.polygonsplot =
function (x, y, z, subscripts, at = pretty(z), shrink, labels = NULL, 
   		label.style = c("mixed", "flat", "align"), contour = FALSE, 
   		region = TRUE, col = add.line$col, lty = add.line$lty, 
		lwd = add.line$lwd, 
   		cex = add.text$cex, font = add.text$font, 
		fontfamily = add.text$fontfamily, 
   		fontface = add.text$fontface, col.text = add.text$col, ..., 
   		col.regions = regions$col, alpha.regions = regions$alpha, 
		grid.polygons, sp.layout) 
{
	regions <- trellis.par.get("regions")
	add.line <- trellis.par.get("add.line")
	add.text <- trellis.par.get("add.text")
	numcol <- length(at) - 1
	numcol.r <- length(col.regions)
	col.regions <- if (numcol.r <= numcol) 
   			rep(col.regions, length = numcol)
   		else col.regions[floor(1 + (1:numcol - 1) * (numcol.r - 1)/(numcol - 1))]
	zcol <- rep(NA, length(z))
	for (i in seq(along = col.regions)) zcol[!is.na(x) & !is.na(y) & 
      			!is.na(z) & z >= at[i] & z < at[i + 1]] <- i
	label.style <- match.arg(label.style)
	x <- as.numeric(x[subscripts])
	y <- as.numeric(y[subscripts])
	z <- as.numeric(z[subscripts])
	zcol <- as.numeric(zcol[subscripts])

	sppanel(list(sp.layout), panel.number(), first = TRUE)
	if (any(subscripts)) {
		if (is(grid.polygons, "SpatialLines")) {
			sp.lines3 = function(x, col, ...) panel.lines(coordinates(x), col = col, ...)
			sp.lines2 = function(x, col, ...) lapply(x@Lines, sp.lines3, col, ...)
			for (i in 1:length(grid.polygons@lines))
				sp.lines2(grid.polygons@lines[[i]], col = col.regions[zcol[i]], lwd = lwd, lty = lty, ...)
		} else {
			pls = slot(grid.polygons, "polygons")
   			pO = slot(grid.polygons, "plotOrder")
			col = rep(col, length.out = length(grid.polygons))
   			for (i in pO) {
				if (get_Polypath()) {
					obj = as(as(grid.polygons[i,], "SpatialLines"),
							"SpatialPointsDataFrame")
					cc = coordinates(obj)
					id = as.numeric(obj$Line.NR)
					fill = col.regions[zcol[i]]
					alpha = alpha.regions
					grid.path(cc[,1], cc[,2], id, default.units = "native",
						gp = gpar(col = col[i], fill = fill, alpha = alpha, 
							lwd = lwd, lty = lty, ...))
				} else {
       				Srs <- slot(pls[[i]], "Polygons")
       				pOi <- slot(pls[[i]], "plotOrder")
       				for (j in pOi) {
						coords = slot(Srs[[j]], "coords")
						if (slot(Srs[[j]], "hole")) {
							bg = trellis.par.get()$background
							if (bg$col == "transparent")
								fill = "white"
							else
								fill = bg$col
							alpha = bg$alpha
						} else {
							fill = col.regions[zcol[i]]
							alpha = alpha.regions
						}
						gp = gpar(fill = fill, alpha = alpha, col = col, lwd = lwd, lty = lty)
						grid.polygon(coords[,1], coords[,2], default.units = "native", 
							gp = gp)
					}
				}
   			}
		}
	}
	sppanel(list(sp.layout), panel.number(), first = FALSE)
}

panel.pointsplot = function(sp.layout, x, y, subscripts, groups, col, cex,
		pch, ...) {
	sppanel(list(sp.layout), panel.number(), first = TRUE)
	lpoints(x, y, fill = groups[subscripts], col = col[subscripts], 
		cex = cex[subscripts], pch = pch[subscripts], ...)
	sppanel(list(sp.layout), panel.number(), first = FALSE)
}

SpatialPolygons2Grob = function(obj, fill) {
	if (!is(obj, "SpatialPolygons"))
		stop("object is not of class SpatialPolygons")
	x = numeric(0)
	y = numeric(0)
	id = integer(0)
	pls = slot(obj, "polygons")
   	pO <- slot(obj, "plotOrder")
	n = 0
   	for (i in pO) {
   		Srs <- slot(pls[[i]], "Polygons")
   		pOi <- slot(pls[[i]], "plotOrder")
   		for (j in pOi) {
			n = n + 1
			cc = slot(Srs[[j]], "coords")
			x = c(x, cc[,1])
			y = c(y, cc[,2])
			id = c(id, rep(n, nrow(cc)))
		}
	}
	polygonGrob(x=x, y=y, id=id, gp = gpar(fill = fill))
}

SpatialPolygonsRescale = function(obj, offset, scale = 1, fill = "black", col = "black", plot.grid = TRUE, ...) {
	if (!is(obj, "SpatialPolygons"))
		stop("object is not of class SpatialPolygons")
	if (length(offset) != 2)
		stop("offset should have length 2")
	if (is.list(offset))
		offset = c(offset[[1]], offset[[2]])
	if (length(scale) == 1)
		scale = rep(scale,2)
	pls = slot(obj, "polygons")
   	pO = slot(obj, "plotOrder")
	fill = rep(fill, length = length(pls))
   	for (i in pO) {
   		Srs <- slot(pls[[i]], "Polygons")
   		pOi <- slot(pls[[i]], "plotOrder")
   		for (j in pOi) {
			cc = slot(Srs[[j]], "coords")
			x = offset[1] + (cc[,1] * scale[1])
			y = offset[2] + (cc[,2] * scale[2])
			if (plot.grid) {
				grid.polygon(x, y, default.units = "native", 
					gp = gpar(col = col, fill = fill[i], ...))
			} else {
				polygon(x, y, col = fill[i])
				lines(x, y, col = col)
			}
		}
	}
}

mapLegendGrob <- function(obj, widths = unit(1, "cm"), heights = unit(1, "cm"),
		fill = "black", just = "right") {
	grb = SpatialPolygons2Grob(obj, fill)
	key.layout <- grid.layout(nrow = 1, ncol = 1, widths = widths,
					heights = heights, respect = TRUE, just = just)
	key.gf <- frameGrob(layout = key.layout)
	key.gf <- placeGrob(key.gf,
				  rectGrob(gp = gpar(fill = "transparent", col = NULL)),
				  row = NULL, col = NULL)
	key.gf <- placeGrob(key.gf, grb, row = 1, col = 1)
	key.gf
}

layout.north.arrow = function(type = 1) {
	if (type == 1) {
		x1 = c(0.1653, 0.2241, 0.2241, 0.2830, 0.1947, 0.1065, 0.1653, 0.1653)
		x2 = c(0, 0.0967, 0.0967, 0.2928, 0.3908, 0.3908, 0.2928, 0.2928, 0.1032, 0, 0)
		y1 = c(0, 0, 0.8823, 0.8235, 1, 0.8235, 0.8823, 0)
		y2 = c(0.2352, 0.2352, 0.5686, 0.2352, 0.2352, 0.7189, 0.7189, 0.3986, 0.7189, 0.7189, 0.2352 )
		return(SpatialPolygons(list(Polygons(list(Polygon(cbind(x1,y1)), Polygon(cbind(rev(x2),rev(y2)))), ID="north"))))
	}
	if (type == 2) {
		x = c(0.143,0.143,0.0143,0.207,0.400,0.271,0.271,0.143)
		y = c(0,0.707,0.707,0.964,0.707,0.707,0.00,0.0)
		return(SpatialPolygons(list(Polygons(list(Polygon(cbind(x,y))), ID="north"))))
	}
	stop("unknown value for type")
}

layout.scale.bar = function(height = 0.05) {
	x1 = c(0, 0.5, 0.5, 0, 0)
	y1 = c(0, 0, height, height, 0)
	x2 = x1 + 0.5
	y2 = y1
	SpatialPolygons(list(Polygons(list(Polygon(cbind(x1,y1))), ID="left"), 
			Polygons(list(Polygon(cbind(rev(x2),rev(y2)))), ID="right")))
}
# scale.bar = .scale.bar()

sp.theme = function(set = FALSE, regions = list(col = bpy.colors(100)), ...) {
	lst = list(regions = regions, ...)
	if (set)
		trellis.par.set(lst)
	else
		lst
}

spplot.key = function(sp.layout, rows = 1, cols = 1) {
	for (i in seq(along=rows)) {
		for (j in seq(along=cols)) {
			trellis.focus("panel", cols[j], rows[i], highlight = FALSE)
			sppanel(sp.layout)
			trellis.unfocus()
		}
	}
}

#sp.pagefn = function(n) {
#	pos = lattice:::lattice.getStatus("current.panel.positions")
#	spplot.key(sp.layout, pos[1], pos[2])
#}

longlat.scales = function(obj, scales, xlim, ylim) {
	isp = is.projected(obj)
	if (!is.null(scales$draw) && scales$draw && !is.na(isp) && !isp) {
		# long lat -- x:
		if (is.null(scales$x))
			scales$x = list()
		if (is.null(scales$x$at))
			scales$x$at = pretty(xlim)
		if (is.null(scales$x$labels))
        	scales$x$labels = parse(text = degreeLabelsEW(scales$x$at))
		# long lat -- y:
		if (is.null(scales$y))
			scales$y = list()
		if (is.null(scales$y$at))
			scales$y$at = pretty(ylim)
		if (is.null(scales$y$labels))
        	scales$y$labels = parse(text = degreeLabelsNS(scales$y$at))
	}
	scales
}

bbexpand = function(x, fraction) {
	r = diff(x)
	c(x[1] - fraction * r, x[2] + fraction * r)
}

colorkey.factor = function(f, colorkey = list(), doColorkey = TRUE) {
	lf = levels(f)
	at = seq(0.5, nlevels(f)+0.501)
	at.labels = seq(1, nlevels(f))
	if (doColorkey) {
		colorkey=append(colorkey, list(labels=list(at=at.labels, labels=lf), 
			height=min(1, .05 * length(lf))))
		list(at = at, colorkey = colorkey)
	} else
		list(at = at)
}

"spplot.locator" <- function(n = 512, type = "n", ...) { 
	stopifnot(n > 0)
	res = as.numeric(grid.locator(unit = "native"))
	if (type == "o" || type == "p")
		panel.points(res[1], res[2], ...)
	if (n > 1) for (i in 2:n) {
		xy = grid.locator(unit = "native")
		if (is.null(xy))
			# return(res)
			break
		else
			xy = as.numeric(xy)
		res = rbind(res, xy)
		if (type == "o" || type == "p")
			panel.points(xy[1], xy[2], ...)
		if (type == "o" || type == "l")
			panel.lines(res[(i-1):i,])
	}
	if (is.matrix(res))
		dimnames(res) = list(NULL, NULL)
	res
}

addNAemptyRowsCols = function(obj) {
	# accept gridded; return SpatialPointsDataFrame with NA records on empty row/cols
	fullgrid(obj) = FALSE
	nfull = obj@grid@cells.dim[1] * obj@grid@cells.dim[2]
	missingpatt = rep(TRUE, nfull)
	missingpatt[obj@grid.index] = FALSE
	missingpatt = matrix(missingpatt,
		obj@grid@cells.dim[1], obj@grid@cells.dim[2], byrow = FALSE)
	missing.x = which(apply(missingpatt, 1, all))
	missing.y = which(apply(missingpatt, 2, all))

	xy = coordinates(obj)[,1:2,drop=FALSE]
	coordvals = coordinatevalues(obj@grid)
	missing.x = coordvals[[1]][missing.x]
	missing.y = coordvals[[2]][missing.y]
	n = length(missing.x) + length(missing.y)
	if (n > 0) {
		if (length(missing.x) > 0)
			xy = rbind(xy, cbind(missing.x, rep(xy[1,2], length(missing.x))))
		if (length(missing.y) > 0)
			xy = rbind(xy, cbind(rep(xy[1,1], length(missing.y)), missing.y))
		newatt = data.frame(lapply(obj@data, function(x) c(x, rep(NA, n))))
		row.names(xy) = seq_len(nrow(xy)) 
		obj = SpatialPointsDataFrame(xy, newatt, obj@coords.nrs, obj@proj4string, FALSE)
	} else
		obj = as(obj, "SpatialPointsDataFrame")
	obj
}

fill.call.groups <-
function (lst, z, ..., cuts = ifelse(identical(FALSE, colorkey), 5, 100), 
	#col.regions = trellis.par.get("regions")$col, 
    legendEntries = "", pch, cex = 1, do.fill = TRUE, do.log = FALSE, 
    key.space = ifelse(identical(FALSE, colorkey), "bottom", "right"), 
	cex.key, edge.col, colorkey) 
{
    dots = list(...)
	col.regions = lst$col.regions
    if (is.numeric(z)) {
        if (length(cuts) > 1) 
            ncuts = length(cuts) - 1
        else ncuts = cuts
        if (ncuts != length(col.regions)) {
            cols = round(1 + (length(col.regions) - 1) * (0:(ncuts - 
                1))/(ncuts - 1))
            fill = col.regions[cols]
        } else 
			fill = col.regions
        valid = !is.na(z)
        if (length(cuts) == 1) {
            if (do.log) {
                lz = log(z)
                cuts = c(min(z[valid]), exp(seq(min(lz[valid]), 
                  max(lz[valid]), length = cuts + 1))[2:(cuts)], 
                  max(z[valid]))
            }
            else cuts = seq(min(z[valid]), max(z[valid]), length = cuts + 
                1)
        }
        groups = cut(as.matrix(z), cuts, dig.lab = 4, include.lowest = TRUE)
    } else if (is.factor(z)) {
        if (length(col.regions) == 1) 
            col.regions = rep(col.regions, nlevels(z))
        if (length(col.regions) < nlevels(z)) 
            stop("number of colors smaller than number of factor levels")
        if (length(col.regions) > nlevels(z)) {
            ncuts = nlevels(z)
            cols = round(1 + (length(col.regions) - 1) * (0:(ncuts - 
                1))/(ncuts - 1))
            col.regions = col.regions[cols]
        }
        if (!missing(cuts)) 
            stop("ncuts cannot be set for factor variable")
        groups = z
		fill = col.regions
    } else stop("dependent of not-supported class")
    n = nlevels(groups)

	# deal with col:
	lst$groups = fill[groups]
	#print(lst$col)

	# deal with pch:
	if (edge.col != "transparent") { # WITH border: use fill
    	if (missing(pch)) 
        	pch = rep(ifelse(do.fill, 21, 1), n)
		lst$col = rep(edge.col, length.out = length(groups))
	} else { # no border: use col instead of fill
    	if (missing(pch)) 
        	pch = rep(ifelse(do.fill, 16, 1), n)
		lst$col = lst$groups
	}

	if (length(pch) == 1)
		pch = rep(pch, n)
	lst$pch = pch[groups]

	# deal with cex:
	if (missing(cex))
		cex = rep(1, n)
	if (length(cex) == 1)
		cex = rep(cex, n)
	if (length(cex) == n) {
		cex.key = cex
		lst$cex = cex[groups]
	} else if (missing(cex.key))
		cex.key = mean(cex, na.rm = TRUE)

	# do key:
	if (is.list(colorkey))
		lst$legend = colorkey
	else if (isTRUE(colorkey)) {
		lst$legend = list(
			right = list(
				fun = draw.colorkey,
                args = list(
					key = list(
						col = col.regions, 
						at = cuts
					), 
                    draw = FALSE
				)
			)
		)
       	if (is.character(key.space)) 
			names(lst$legend) = key.space
	} else {
    	if (!identical(dots$auto.key, FALSE)) { # xxx
    		if (missing(legendEntries)) 
				legendEntries = levels(groups)
        	if (!is.null(dots$key)) 
            	lst$key = dots$key
			else { 
				if(is.list(dots$auto.key))
					lst$key = dots$auto.key
				else
					lst$key = list()
				if (edge.col != "transparent") {
					lst$key = append(lst$key,
						list(points = list(
							pch = rep(pch, length.out = n), 
							col = rep(edge.col, length.out = n), 
							fill = fill, 
							cex = rep(cex.key, length.out = n)
						), 
						text = list(legendEntries)
					))
				} else {
					lst$key = append(lst$key,
						list(points = list(
							pch = rep(pch, length.out = n), 
							col = rep(fill, length.out = n), 
							cex = rep(cex.key, length.out = n)
						), 
						text = list(legendEntries)
					))
				}
			}
        	if (is.character(key.space)) 
            	lst$key$space = key.space
        	else if (is.list(key.space)) 
            	lst$key = append(lst$key, key.space)
        	else warning("key.space argument ignored (not list or character)")
			# print(lst$key)
    	}
    	if (!is.null(dots$auto.key)) 
        	lst$auto.key <- dots$auto.key
	}
    return(lst)
}

panel.RgoogleMaps <- function(map) {
	bb = bb2merc(map, "RgoogleMaps")
	grid.raster(map$myTile, mean(bb[1,]), mean(bb[2,]), diff(bb[1,]), diff(bb[2,]), 
		default.units = "native", interpolate = FALSE)
}

panel.ggmap <- function(map) {
	bb = bb2merc(map, "ggmap")
	grid.raster(map, mean(bb[1,]), mean(bb[2,]), diff(bb[1,]), diff(bb[2,]), 
		default.units = "native", interpolate = FALSE)
}
