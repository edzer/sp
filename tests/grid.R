library(sp)
data(meuse.grid)
x = meuse.grid
coordinates(x) = c("x", "y")
gridded(x) = TRUE
gridded(x)
image(x["dist"])

fullgrid(x) = TRUE
fullgrid(x)
summary(x)
gridparameters(x)
class(as(x, "matrix"))

fullgrid(x) = FALSE
fullgrid(x)
summary(x)
class(as(x, "matrix"))
gridparameters(x)

df = data.frame(z = c(1:6,NA,8,9), 
	xc = c(1,1,1,2,2,2,3,3,3), 
	yc = c(rep(c(0, 1.5, 3),3)))

coordinates(df) = ~xc+yc
gridded(df) = TRUE
gridparameters(df)
as(df, "matrix")

# get grid topology:
grd = points2grid(as(df, "SpatialPoints"), 1e-31) 
grd
getGridIndex(coordinates(df), grd)

g = SpatialGrid(grid = grd)
fullgrid(g)
fullgrid(g) = TRUE
class(g)
# the next one has to fail:
fullgrid(g) <- FALSE
class(g)

print(summary(df))
image(df["z"])
as.image.SpatialGridDataFrame(df)
as.image.SpatialGridDataFrame(df["z"])
coordinatevalues(getGridTopology(df))

as.data.frame(df)

fullgrid(df) = TRUE
as.data.frame(df)

fullgrid(df) = FALSE  
as.data.frame(df)

fullgrid(df) = TRUE
fullgrid(df) = FALSE
as.data.frame(df)

df = as.data.frame(df)
set.seed(133331)
df$xc = df$xc + rep(.001*rnorm(3), 3)[1:8]
df.sp = SpatialPoints(df[c("xc", "yc")])
df.grd = SpatialPixels(df.sp, tolerance = .01)
df.grd[1:4,,tolerance=.01,drop=TRUE]
df.grd[1:4,,tolerance=.01]
