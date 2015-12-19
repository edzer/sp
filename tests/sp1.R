library(sp)
data(meuse)
x = meuse
nm <- names(meuse)
# rename to non-default names:
nm[1] <- "xcoord"
nm[2] <- "ycoord"
names(x) <- nm
# change column order
x = x[ , c(3:14,2,1)] 
coordinates(x) <- c("xcoord", "ycoord") # columns named xcoord and ycoord
coordinates(x)[1:10,] 
meuse[1:10,] 
class(x)
sum = summary(x)
print(sum)
coordinates(x)

x <- as.data.frame(x)
class(x)

x[1:10, c("xcoord", "ycoord")]

x = meuse[1:4,]
coordinates(x) = c(1,2)
# row 1,2; cols 1:10
x[1:2,1:10]
# row 2, coord+col 9,10
x[2,9:10]
# coordinates, col 9+10
x[,9:10]
# coordinates + col 9:
x[,9]
# coordinates + zinc column:
x["zinc"]
# second row, coordinates + zinc
x[2,"zinc"]
# select; re-orders:
x[c("zinc","copper")]
# back as data.frame
as.data.frame(x)[1:3, c("zinc","copper","x", "y")]
