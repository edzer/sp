library(sp)

g = SpatialGrid(GridTopology(c(0,0), c(1,1), c(3,3)))
p = as(g, "SpatialPolygons")
over(g,g)
over(p,p)
over(p,p, minDimension = 0) # orders; different names
over(p,g)
over(g,p)

over(p,p,returnList=TRUE)
over(p,p,returnList=TRUE, minDimension=0)
over(p,p,returnList=TRUE, minDimension=1)
over(p,p,returnList=TRUE, minDimension=2)
over(p,p[1:6],minDimension=2)

x2 = x1 = cbind(c(0,1,1,0,0), c(0,0,1,1,0))
x1[,1] = x1[,1]+0.5
x1[,2] = x1[,2]+0.25
sp = SpatialPolygons(list(
 Polygons(list(Polygon(x1)), "x1"),
 Polygons(list(Polygon(x2)), "x2")))
pt = SpatialPoints(cbind(0.5,0.5)) # on border of x1
row.names(pt) = "pt1"
over(pt,sp)
over(pt,sp,returnList=TRUE)

rgeos::overGeomGeom(pt,sp)
rgeos::overGeomGeom(pt,sp,returnList=TRUE)

plot(sp)
plot(pt,add=TRUE,col='red',pch=16)
#    x1     x2 
#	"F0FF" "0FFF" 
# it would be nice to have these sorted "2, 1" instead of "1, 2" - use
rgeos::overGeomGeom(pt,sp,returnList=TRUE, minDimension = 0)

rgeos::overGeomGeom(pt,pt,minDimension=2)
rgeos::overGeomGeom(pt,pt,minDimension=1)
rgeos::overGeomGeom(pt,pt,minDimension=0)
