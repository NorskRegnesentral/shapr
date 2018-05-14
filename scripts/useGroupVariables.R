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


groupedSim = group_variables(corMat = corMat, alpha = 1)

## DNB data
trainData = read.table("/nr/project/stat/BigInsight/Projects/Explanations/Data/train6.csv",sep=";",header=TRUE)
## Remove response
trainData = trainData[,-1]
## Kendall's tau
corMatDNB = pcaPP::cor.fk (trainData)

## Simplify labels
rown = unlist(strsplit(dimnames(corMatDNB)[[1]],"_end"))
rown = unlist(strsplit(rown,"_correct"))
dimnames(corMatDNB) = list(rown,rown)

groupedDNB = group_variables(corMat = corMatDNB, alpha = 0.1)
