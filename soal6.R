#6.a
dataset <- rnorm(100, 50, 8)
zscore <- (dataset - 50)/sd(dataset)
plot(dataset)
print(paste("6.a) Z: ", zscore), quote=FALSE)

#6.b
hist(rnorm(100, 50, 8), main="5025201195_Naufal_Probstat_A_DNhistogram")

#6.c
print(paste("6.c) Var[x]: ", sd(dataset) ** 2), quote=FALSE)
