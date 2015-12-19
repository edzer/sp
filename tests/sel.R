library(sp)
x = c(0.5, 1.5, 0.5, 1.5, 1.6)
y = c(1.5, 1.5, 0.5, 0.5, 0.5)
xy = cbind(x,y)
dimnames(xy)[[1]] = c("a", "b", "c", "d", "e")
pts = SpatialPoints(xy)
z = data.frame(z1 = 1:5, z2=5:1, f = c("a", "a", "b", "b", "b"))
row.names(z) = c("a", "b", "c", "d", "e")
ptsdf = SpatialPointsDataFrame(pts, z)

xpol = c(0,1,1,0,0)
ypol = c(0,0,1,1,0)
pol = SpatialPolygons(list(
	Polygons(list(Polygon(cbind(xpol-1,ypol))), ID="x9"),
	Polygons(list(Polygon(cbind(xpol,ypol))), ID="x2"),
	Polygons(list(Polygon(cbind(xpol,ypol-1))), ID="x3"),
	Polygons(list(Polygon(cbind(xpol+1,ypol))), ID="x4")
	))
z = data.frame(z = c(10, 15, 25, 3), zz = 1:4, f = c("z", "q", "r", "z"), 
	row.names = c("x9", "x2", "x3", "x4"))
poldf = SpatialPolygonsDataFrame(pol, z)
plot(pol, xlim = c(-1.5, 2))
points(pts, col='red')

over(pts, pol)
over(pts, poldf)
over(pts, poldf[1:2], fn = mean)

#rbind(poldf, over(pts, poldf[1:2], fn = mean))

over(pol, pts)
over(pol, ptsdf)
over(pol, ptsdf[1:2], fn = mean)

pts[pol]

points(pts[pol], col='green', pch=16, cex=.8)
summary(pol[pts])
plot(pol[pts], border='blue', add=TRUE)

gt = GridTopology(c(.5,.5), c(1,1), c(3,2))
sg = SpatialGrid(gt)
df6 = data.frame(z = 6:1, f = c("a", "a", "b", "b", "c", "c"))
sgdf = SpatialGridDataFrame(gt, df6)
over(sg, pol)
over(sg, poldf)
over(sg, poldf[1:2])

spix = as(sg, "SpatialPixels")
spixdf = as(sgdf, "SpatialPixelsDataFrame")
over(spix, pol)
over(spix, poldf)
over(spix, poldf[1:2])

over(pol, sg)
over(pol, sgdf)
over(pol, sgdf[1], fn = mean)

over(pol, spix)
over(pol, spixdf)
over(pol, spixdf[1], fn = mean)

over(pts, sg)
over(pts, spix)
over(pts, sgdf)
over(pts, spixdf)

#over(pts, sg, returnList=TRUE)
#over(pts, spix, returnList=TRUE)
#over(pts, sgdf, returnList=TRUE)
#over(pts, spixdf, returnList=TRUE)
