

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(Rcpp)

m <- 15

Xtrain <- rmvnorm(10000,mean = rep(0,m))
Xtest <- rmvnorm(100,mean = rep(0,m))

sourceCpp("src/distance.cpp")

X <- get_combinations(m = m, exact = FALSE, nrows = 5e2)


aa=Mahlanobis_dist_cpp(X$features,Xtrain,Xtest,mcov=cov(Xtrain))


############

D.Rcpp.old=distance_cpp(Xtrain = Xtrain,Xtest = Xtest) # Slightly faster than D.Rcpp.new, but nothing to care about

D.Rcpp.new=prepare_gen_Mahlanobis_dist_cpp(Xtrain = Xtrain,Xtest = Xtest,mcov = diag(p))

D.inR.new=array(dim=c(nrow(Xtrain),nrow(Xtest),p))
    mcov = diag(p)
    dec <- chol(mcov)
    for (i in 1:nrow(Xtest)){ # Rewrite to Rcpp
        D.inR.new[,i,] <- t(forwardsolve(t(dec), t(Xtrain) - unlist(Xtest[i,]) )^2)
    }

all.equal(D.Rcpp.old,D.Rcpp.new)
all.equal(D.Rcpp.old,D.inR.new)



