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
sum(out2 - out)
summary(as.vector(out2 - out))

out = spDists(meuse.grid,meuse)
sum(out2 - t(out))
summary(as.vector(out2 - t(out)))
