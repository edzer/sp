library(sp)
g = SpatialGrid(GridTopology(c(5,5), c(10,10), c(3,3)))
p = as(g, "SpatialPolygons")
p$z = c(1,0,1,0,1,0,1,0,1)
cc = coordinates(g)
p$ag1 = aggregate(p, p, mean)[[1]]
p$ag1a = aggregate(p, p, mean, minDimension = 0)[[1]]
p$ag2 = aggregate(p, p, mean, minDimension = 1)[[1]]
p$ag3 = aggregate(p, p, mean, minDimension = 2)[[1]]
p$ag4 = aggregate(p, p, mean, areaWeighted=TRUE)[[1]]
pts = cbind(c(9,21,21,9,9),c(9,9,21,21,9))
sq = SpatialPolygons(list(Polygons(list(Polygon(pts)), "ID")))
rnd2 = function(x) round(x, 2)
l = list(
	list("sp.text", cc, rnd2(p$z), which = 1),
	list("sp.text", cc, rnd2(p$ag1), which = 2),
	list("sp.text", cc, rnd2(p$ag1a), which = 3),
	list("sp.text", cc, rnd2(p$ag2), which = 4),
	list("sp.text", cc, rnd2(p$ag3), which = 5),
	list("sp.text", cc, rnd2(p$ag4), which = 6),
	list(sq, col = 'green', which = 6, first = FALSE, lwd = 2)
)
spplot(p, names.attr = c("source", "default aggregate", "minDimension=0", 
	"minDimension=1", "minDimension=2", "areaWeighted=TRUE"), layout = c(3,2), 
	as.table=TRUE, col.regions=bpy.colors(151)[50:151], cuts=100, 
	sp.layout = l, scales = list(draw = TRUE))

rnd2(c(aggregate(p, sq, mean)[[1]],
  aggregate(p, sq, mean, minDimension = 0)[[1]],
  aggregate(p, sq, mean, minDimension = 1)[[1]],
  aggregate(p, sq, mean, minDimension = 2)[[1]],
  aggregate(p, sq, mean, areaWeighted=TRUE)[[1]]))
