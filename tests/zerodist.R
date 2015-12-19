library(sp)
data(meuse)
# pick 10 rows
n = 10
set.seed(1357) # fix seed for exact reproduction of test:
ran10 = sample(nrow(meuse), size = n, replace = FALSE)
meusedup = rbind(meuse, meuse[ran10, ])
coordinates(meusedup) = c("x", "y")
zd = zerodist(meusedup)
sum(abs(sort(zd[,1]) - sort(ran10))) # 0!
zerodist(meusedup, unique.ID = TRUE)
# remove the duplicate rows:
meusedup2 = meusedup[-zd[,2], ]
print(summary(meusedup2))
meusedup3 <- subset(meusedup, !(1:nrow(meusedup) %in% zd[,2]))
print(summary(meusedup3))
dim(meuse)
dim(meusedup2)
dim(meusedup3)
dim(remove.duplicates(meusedup))
