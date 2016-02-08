library(sp)
data(meuse)
x = meuse[1:10, ] # limit the output
coordinates(x) = c("x", "y") # names
bbox(x)
is.projected(x)
dimensions(x)
x

x = meuse[1:10, ]
coordinates(x) = c(1, 2) # coordinate column numbers
x

x = meuse[1:10, ]
coordinates(x) = ~x+y # coordinates formula
x

x = meuse[1:10, ]
coordinates(x) = meuse[1:10, c("x", "y")] # coords, as data.frame
x

x = meuse[1:10, ]
coordinates(x) = as.matrix(meuse[1:10, c("x", "y")]) # coords, as matrix
x

x = meuse[1:20,]
coordinates(x) = c("x", "y") # coordinate column names
print(summary(x))

x[1:10] # first 10 columns
x[, 1:10] # first 10 columns
x[1:10,] # rows 1-10
x["zinc"] # column zinc + coords
x[, "zinc"] # idem
x[1:10, "zinc"] # idem
x[1:10, c("zinc", "cadmium")] # idem
x[["zinc"]]
x[["lnzinc"]] <- log(x[["zinc"]])
x

print(summary(x[1:10, "zinc"])) # check bbox
print(summary(x["zinc"])) # compare bbox

data(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE
plot(meuse.grid)
