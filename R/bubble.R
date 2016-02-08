# Colorbrewer.org, 5-class PiYG
#d01c8b
#f1b6da
#f7f7f7
#b8e186
#4dac26

"bubble" <- function (obj, zcol = 1, ..., fill = TRUE, maxsize = 3, do.sqrt = TRUE, 
	pch, col = c("#d01c8b", "#4dac26"), key.entries = quantile(data[,zcol]),
	main = ifelse(is.numeric(zcol), names(data)[zcol], zcol),
    identify = FALSE, labels = row.names(data.frame(obj)), key.space = "right",
	scales = list(draw = FALSE), xlab = NULL, ylab = NULL, panel = panel.bubble,
	sp.layout = NULL, 
	xlim = bbexpand(bbox(obj)[1,], 0.04), 
	ylim = bbexpand(bbox(obj)[2,], 0.04))
{
	obj = as(obj, "SpatialPointsDataFrame")
	data = as.data.frame(obj)
	cc = coordinates(obj)
    x = cc[, 1]
    y = cc[, 2]
	if (NCOL(data) == 1)
		z = data
	else if (NCOL(data) == 0)
		z = rep(1, NROW(cc))
	else
    	z = data[, zcol]
    if (missing(pch)) 
        pch = ifelse(fill, 16, 1)
	if (length(col) == 1)
		col = rep(col, 2)
    z.col = as.vector(ifelse(z < 0, col[1], col[2]))
    q = key.entries
    q.pch = rep(pch, length(q))
    q.text = as.character(round(q, 3))
    q.col = as.vector(ifelse(q < 0, col[1], col[2]))
    az = abs(z)
    q = abs(q)
    if (do.sqrt) {
		az = sqrt(az)
		q = sqrt(q)
    }
    cex = as.vector(maxsize * az/max(az,q))
    q.cex = as.vector(maxsize * q/max(az,q))

	scales = longlat.scales(obj, scales, xlim, ylim)
    if (identify) {
		plot(x, y, asp = 1, cex = cex, main = main, ...)
		return(identify(x, y, labels))
	} else {
    	key = list(space = key.space, points = list(pch = q.pch, col = q.col, 
    		cex = q.cex), text = list(q.text))
		xyplot(y ~ x, col = z.col, cex = cex, pch = pch, asp = mapasp(obj), 
        	key = key, main = main, scales = scales, xlab = xlab, ylab = ylab,
			panel = panel, sp.layout = sp.layout, ...)
	}
}

panel.bubble = function(x, y, subscripts, sp.layout, ...) {
	# sp.panel.layout(sp.layout, panel.number())
	sppanel(sp.layout, panel.number())
	panel.xyplot(x, y, ...)
}
