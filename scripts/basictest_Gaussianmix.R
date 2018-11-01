library(mclust)

# Select 4 continuous variables and look for three distinct groups.
mcl.model <- Mclust(iris[, 1:4], 1:9)


plot(mcl.model)
4


test=Mclust(Xtrain,1:9)

plot(test)
