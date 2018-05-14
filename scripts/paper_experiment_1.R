# Paper example 1:

rm(list = ls())

library(shapr)
library(data.table)

source("scripts/paper_helper_funcs.R")

samp_variables <- function(n){
    mu.list = list(c(0,0,0))
    Sigma.list <- list(matrix(c(1,0.7,0.7,
                                0.7,1,0.7,
                                0.7,0.7,1),ncol=3))
    pi.G <- 1

    X <- joint.samp.func(n = n,
                    pi.G,
                    mu.list,
                    Sigma.list)
    return(X[])
}

samp_model <- function(n,X){
    y <- X[,1] + X[,2] + X[,3] + rnorm(n = n,mean=0,sd=0.1)
}



nTrain <- 10000
nTest <- 100

set.seed(123)
Xtrain <- data.table(samp_variables(nTrain))
Xtrain <- Xtrain[,y:=samp_model(.N,.SD)]

Xtest <- data.table(samp_variables(nTest))
Xtest <- Xtest[,y:=samp_model(.N,.SD)]

