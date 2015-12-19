library(sp)

# open polygon:
print(point.in.polygon(1:10,1:10,c(3,5,5,3),c(3,3,5,5)))
# closed polygon:
print(point.in.polygon(1:10,rep(4,10),c(3,5,5,3,3),c(3,3,5,5,3)))
