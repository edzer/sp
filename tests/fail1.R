library(sp)
data(meuse)
x = meuse

coordinates(x) <- c("x", "y")
try(proj4string(x) <- 1.5)
try(coordinates(a) <- cbind(1:10, 10:1))
# fails because a is not found; passes if a assigned NULL, see pass1.R

x = meuse
# invalid coordinate formulae:
try(coordinates(x) <- ~log(x)+sqrt(y)) # no expressions allowed
try(coordinates(x) <- ~x+y+z) # z is not present
x$x2 = x$x^2
x$y2 = x$y^2
try(coordinates(x) <- ~x+y+x2+y2) # 4D now passes check...
x = meuse
try(coordinates(x) <- ~x) # 1D not allowed

# is.na.sp.coords
a = data.frame(cbind(xx=c(1,NA,2,10),yy=c(2,NA,NA,20)))
try(coordinates(a) <- c("xx", "yy")) # should fail!

x = meuse[1:4,]
coordinates(x) = c(1,2)
# this should fail -- zinc is not a row:
#(and will break automatic testing, so outcommented!)
#try(q <- x["zinc",])
# this will issue warning under S-Plus, or a silent rename under R
try(x[c("zinc", "copper", "zinc")])

# this will fail, as "x" is not in the data part:
try(x[c("zinc", "x", "copper", "zinc")])

# row index containing missing values will fail:
try(xx <- x[c(1:3,NA),])

xx = data.frame(x=1:10, y=1:10)

# fails; use SpatialPoints() to create points without attribute 
try(coordinates(xx) <- c("x", "y")) 

x = matrix(3, 5, 2)
dimnames(x) = list(c(1,1:4), NULL)
y = data.frame(a = 1:5, b = 5:1)
try(SpatialPointsDataFrame(x, y)) # will complain:
SpatialPointsDataFrame(x, y, match.ID = FALSE) # won't complain

Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s2")
try(SR <- SpatialPolygons(list(Srs1,Srs2,Srs3))) # will complain
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SR = SpatialPolygons(list(Srs1,Srs2,Srs3)) # won't complain
try(SRx <- SR[c(1,2,NA),])

attr = data.frame(a=1:3, b=3:1, row.names=c("s1", "s2", "s3"))
try(SrDf <- SpatialPolygonsDataFrame(SR, attr)) # will complain
SrDf = SpatialPolygonsDataFrame(SR, attr, match.ID = FALSE) # won't complain
attr = data.frame(a=1:3, b=3:1, row.names=c("s1", "s2", "s3/4"))
SrDf = SpatialPolygonsDataFrame(SR, attr) # won't complain

l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
S3 = Lines(list(Sl2), ID="a")
Sl = SpatialLines(list(S1,S2)) # won't complain
try(Sl1 <- SpatialLines(list(S1,S3))) # will complain
try(Sl1 <- Sl[c(NA,2),]) # will fail

df = data.frame(z = c(1,2), row.names=sapply(slot(Sl, "lines"), function(x) slot(x, "ID")))
Sldf = SpatialLinesDataFrame(Sl, data = df) # won't complain
df1 = data.frame(z = c(1,2))
try(Sldf1 <- SpatialLinesDataFrame(Sl, data = df1)) # will complain
Sldf1 = SpatialLinesDataFrame(Sl, data = df1, match.ID = FALSE) # won't complain
try(Sldf1 <- Sldf1[c(1,NA),])

data(meuse.grid)
gridded(meuse.grid) = ~x+y
try(x <- meuse.grid[c(1:10,NA,12),])
fullgrid(meuse.grid) = TRUE
try(x <- meuse.grid[c(1:10,NA,12),])

try(x <- meuse[[c("zinc", "cadmium")]])
try(meuse[[c("zn", "cd")]] <- cbind(meuse$zinc, meuse$cadmium))

data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- TRUE
gridparameters(meuse.grid)

image(meuse.grid)
image(meuse.grid[2])
image(meuse.grid, 2)
try(image(meuse.grid, 0))
image(meuse.grid[3], breaks=c(0,.2,.5,.8,1), col = bpy.colors(4))
image(meuse.grid, 3, zlim = c(0,.3))
image(meuse.grid, 3, zlim = c(.3,.1))
image(meuse.grid, 3, zlim = c(.2,.8))
image(meuse.grid, 3, zlim = c(.2,.8), breaks = c(.2,.4,.6,.8), 
	col = bpy.colors(3))

data(meuse.grid)
set.seed(1)
meuse.grid$x <- meuse.grid$x + rnorm(length(meuse.grid$x), 0, 0.0002)
meuse.grid$y <- meuse.grid$y + rnorm(length(meuse.grid$y), 0, 0.0002)
coordinates(meuse.grid) <- c("x", "y")
try(gridded(meuse.grid) <- TRUE)
try(meuse.grid <- SpatialPixelsDataFrame(as(meuse.grid, "SpatialPoints"),
  data=as(meuse.grid, "data.frame"), tolerance=0.077))
gridparameters(meuse.grid)

data(meuse.grid_ll)
try(gridded(meuse.grid_ll) <- TRUE)
try(meuse.grid_ll <- SpatialPixelsDataFrame(as(meuse.grid_ll, "SpatialPoints"), data=as(meuse.grid_ll, "data.frame"), tolerance=0.9))
gridparameters(meuse.grid_ll)

try(CRS("+proj=latlon +ellps=WGS84"))
try(CRS("+proj=lonlat +ellps=WGS84"))
