## Script for test of grouping of variables

## Random correlation matrix
set.seed(11100110)
d = 60
corMat = cor(rockchalk::mvrnorm(n=5,rep(0,d),rockchalk::lazyCor(X=0.2,d=d)))
## Add two independent variables
corMat = rbind(corMat,rep(0,d))
corMat = cbind(corMat,c(rep(0,d),1))
corMat = rbind(corMat,rep(0,d+1))
corMat = cbind(corMat,c(rep(0,d+1),1))

## Dimnames are needed
dimnames(corMat) = list(
    c(paste(1:(d+2))),
    c(paste(1:(d+2)))
)



grouped = group_variables(corMat = corMat, alpha = 1)
