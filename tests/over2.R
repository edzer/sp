options("rgdal_show_exportToProj4_warnings"="none")
library(sp)

g = SpatialGrid(GridTopology(c(0,0), c(1,1), c(3,3)))
p = as(g, "SpatialPolygons")
over(g,g)


