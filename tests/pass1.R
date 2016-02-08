library(sp)
data(meuse)
x = meuse
coordinates(x) = cbind(rnorm(155), rnorm(155))
# should pass:
names(x@data)
names(as.data.frame(x))
class(as(x, "data.frame"))
x = meuse
# coordinates defined as data:
coordinates(x) = cbind(xcoord = rnorm(155), ycoord = rnorm(155))
# should pass:
names(x@data)
names(as.data.frame(x))
is.projected(x)
proj4string(x)

set.seed(13131) # make sample reproducable:
x = meuse[, sample(ncol(meuse))] # 'randomly' shuffle columns
# coordinates defined as variable names:
coordinates(x) = c("x", "y") # no matter their position
#plot(x, cex=.05 * sqrt(x@data[,"zinc"]),
plot(x, cex=.05 * sqrt(as.data.frame(x)[["zinc"]]),pch=1)
title("Meuse: zinc bubble plot")
print(summary(x))

# coordinates defined as formula:
x = meuse[, 1:5]
coordinates(x) = ~x+y
print(summary(x))

# a = NULL
# cc = cbind(sample(1:10), sample(1:10), sample(1:10))
# coordinates(a) = cc
# summary(a)

xx = SpatialPointsDataFrame(matrix(1:10,5,2),data.frame(f = 1:5))
rbind(xx,xx,xx,xx)

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as.SpatialPolygons.GridTopology(grd)
summary(rbind(polys[1:10], polys[11:20], polys[21:30]))
plot(rbind(polys[1:10],polys[21:30]))
title("2 x 10 blocks -- test rbind on SpatialPolygons")

l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
Sl = SpatialLines(list(S1,S2))

summary(as(polys, "SpatialLines"))
summary(as(Sl, "SpatialPoints"))
summary(as(Sl, "SpatialPointsDataFrame"))
SlDf = SpatialLinesDataFrame(Sl, data.frame(xx = c("foo", "bar")), match.ID = FALSE)
summary(as(SlDf, "SpatialPointsDataFrame"))

meuse[["xxx"]] = log(meuse$zinc)
summary(meuse)
meuse$xxy = log(meuse[["zinc"]])
summary(meuse)

# test behaviour on zero-length objects:
demo(meuse, ask = FALSE, echo = FALSE)
p = as(meuse, "SpatialPoints")

p[0]
dim(p[0])

meuse[0,]
dim(meuse[1:3,0])
dim(subset(meuse, zinc < 100, 0))
dim(subset(meuse, , 0))
dim(subset(meuse, F, 0))

dim(meuse.grid[0,])
fullgrid(meuse.grid) = TRUE
dim(meuse.grid[0,])
summary(meuse.grid[0,])
dim(meuse.grid[,0])
summary(meuse.grid[,0])
dim(meuse.grid[0,,0])
summary(meuse.grid[0,,0])

# SpatialPolygons:
L = as(meuse.riv, "SpatialPolygons")
length(L[1])
length(L[0])
summary(L[0])

# SpatialPolygonsDataFrame:
L$something = "bla"
class(L)
length(L[1])
summary(L[1])
length(L[0])
summary(L[0])

# SpatialLines
L = as(meuse.riv, "SpatialLines")
length(L[1])
length(L[0])
summary(L[0])

# SpatialLinesDataFrame
L$something = "bla"
class(L)
length(L[1])
summary(L[1])
length(L[0])
summary(L[0])

m = meuse
all.equal(rbind(m[1:3,], m[5:7,]), rbind(m[1:3,], m[0,], m[5:7,]))
all.equal(rbind(m[1:3,], m[5:7,]), rbind(m[0,], m[1:3,], m[0,], m[5:7,]))
all.equal(rbind(m[1:3,], m[5:7,]), rbind(m[1:3,], m[0,], m[5:7,], m[0,]))

# match.ID settings for SpatialPointsDataFrame():
set.seed(1331)
pts = cbind(1:5, 1:5)
dimnames(pts)[[1]] = letters[1:5]
df = data.frame(a = 1:5)
row.names(df) = letters[5:1]

library(sp)
options(warn=1) # show warnings where they occur
SpatialPointsDataFrame(pts, df) # warn
SpatialPointsDataFrame(pts, df, match.ID = TRUE) # don't warn
SpatialPointsDataFrame(pts, df, match.ID = FALSE) # don't warn
df$m = letters[5:1]
SpatialPointsDataFrame(pts, df, match.ID = "m") # don't warn

dimnames(pts)[[1]] = letters[5:1]
pts
SpatialPointsDataFrame(pts, df) # don't warn: match doesn't reorder

# duplicated row name:
dimnames(pts)[[1]] = letters[c(1:4,1)]
pts
xx = try(x <- SpatialPointsDataFrame(pts, df))
class(xx)
xx
SpatialPointsDataFrame(pts, df, match.ID = FALSE)
try(x <- SpatialPointsDataFrame(pts, df, match.ID = TRUE)) # fail

dimnames(pts)[[1]] = letters[5:1]
row.names(df) = letters[6:2] # non-matching row.names
SpatialPointsDataFrame(pts, df) # do not match.ID
SpatialPointsDataFrame(pts, df, match.ID = FALSE)
try(x <- SpatialPointsDataFrame(pts, df, match.ID = TRUE)) # fail
