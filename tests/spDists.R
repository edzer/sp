options("rgdal_show_exportToProj4_warnings"="none")
library(sp)
data(meuse)
data(meuse.grid)
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
x = coordinates(meuse)
y = coordinates(meuse.grid)
out = spDists(meuse,meuse.grid)
out2 = as.matrix(dist(rbind(coordinates(meuse),coordinates(meuse.grid))))
out2 = out2[1:155,155+1:nrow(y)]
# should be equal:
all.equal(out2, out, check.attributes = FALSE)
out = spDists(meuse.grid, meuse)
all.equal(out2, t(out), check.attributes = FALSE)
