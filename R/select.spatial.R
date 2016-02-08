"select.spatial" <- function(data, digitize = TRUE, pch = "+", 
	rownames = FALSE) 
{
	if (!is(data, "SpatialPoints"))
		stop("data should be of, or extend, class SpatialPoints")
	x = coordinates(data)[, 1]
	y = coordinates(data)[, 2]
	plot(x, y, pch = pch, asp = 1)
	if (rownames && is(data, "SpatialPointsDataFrame"))
		labels = row.names(as.data.frame(data))
	else
		labels = 1:length(x)
	if (digitize) {
		pol = locator(n = 512, type = "o")
		sel = 1:length(x)
		sel = sel[point.in.polygon(x, y, pol$x, pol$y) > 0]
	} else
		sel = identify(x, y, labels = labels)
	labels[sel]
}
