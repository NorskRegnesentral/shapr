

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 1 ####
# Linear model with independent Gaussian features

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,0,0,
                            0,1,0,
                            0,0,1),ncol=3))
pi.G <- 1

sd = 0.1

nTrain <- 10000
nTest <- 10


#### Defining the true distribution of the variables and the model------

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd){
    y <- X[,1] + X[,2] + X[,3] + rnorm(n = n,mean=0,sd=sd)
}




#### Sampling train and test data ---------

set.seed(123)
XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtrain[,y:=samp_model(.N,.SD,sd=sd)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest <- data.table(samp_variables(n = nTest,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtest[,y:=samp_model(.N,.SD,sd=sd)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

#### Fitting the model ----------

model = lm(y~.,data=XYtrain)

pred_zero = XYtrain[, mean(y)]
m = ncol(Xtrain)
exact = TRUE
nrow=1e4

# X <- get_combinations(m = m, exact = exact, nrows = nrows)
# library(Rcpp)
# sourceCpp("src/distance.cpp")
#
# bb=Mahlanobis_dist_cpp(X$features,head(as.matrix(Xtrain)),as.matrix(Xtest[4,]),mcov=cov(Xtrain),T)
# mahalanobis(head(as.matrix(Xtrain))[,c(1,3)],as.matrix(Xtest)[4,c(1,3)],cov(Xtrain)[c(1,3),c(1,3)])/2^2


#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4,
    scale = F,
    distance_metric = "Euclidean")


w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3
sigma = 0.1
verbose = FALSE
gaussian_sample = FALSE


#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

Shapley.approx.test = compute_kernelShap(model = model,
                                         l,
                                         sigma = 0.1,
                                         w_threshold = w_threshold,
                                         n_threshold = n_threshold,
                                         verbose = FALSE,
                                         gaussian_sample = FALSE,
                                         pred_zero=pred_zero,
                                         kernel_metric = "Gaussian_old")





i = 123
j= 8
l$D[i,j,]%*%t(l$S) # The scaled Mahalanobis distance for the 123rd training observations to the 8 test observation

sqrt(l$D[i,j,])%*%matrix(c(0,0,0,0,1,0,0,0,1),ncol=3)%*%t(t(sqrt(l$D[i,j,])))


# The same computed manually... The last number
vec.full = as.matrix(Xtrain)[i,]-as.matrix(Xtest)[j,]
mcov.full <- cov(Xtrain)
t(vec.full)%*%solve(mcov.full)%*%vec.full

# second last
vec = (as.matrix(Xtrain)[i,]-as.matrix(Xtest)[j,])[c(2,3)]
mcov <- cov(Xtrain)[c(2,3),c(2,3)]
t(vec)%*%solve(mcov)%*%vec

S=matrix(c(0,0,0,0,1,0,0,0,1),ncol=3)
t(vec.full)%*%S%*%solve(mcov.full)%*%S%*%vec.full


# Third last
vec = (as.matrix(Xtrain)[i,]-as.matrix(Xtest)[j,])[c(1,3)]
mcov <- cov(Xtrain)[c(1,3),c(1,3)]
(sqrt(t(vec)%*%solve(mcov)%*%vec)/2)^2

mahalanobis(as.matrix(Xtrain)[i,],as.matrix(Xtest)[j,],cov(Xtrain))/9
mahalanobis(as.matrix(Xtrain)[i,],as.matrix(Xtest)[j,],cov(Xtrain))/9





##
# library(Rcpp)
# sourceCpp("src/distance.cpp")
# aa=prepare_gen_Mahlanobis_dist_cpp(Xtrain = as.matrix(Xtrain),Xtest = as.matrix(Xtest[1,]),mcov = diag(m))
# i = 1
# mcov=diag(m)
# dec <- chol(mcov)
# bb <- t(forwardsolve(t(dec), t(as.matrix(Xtrain)) - unlist(Xtest[i,]) )^2)
# sum(aa-bb)
#
#
# aa=prepare_gen_Mahlanobis_dist_cpp(Xtrain = matrix(c(1,2),ncol=1),Xtest = matrix(c(1,2),ncol=1),mcov = as.matrix(1))
#
#
# i = 1
# mcov = matrix(1)
# dec <- chol(mcov)
# bb <- t(forwardsolve(t(dec), t(matrix(c(1,2),ncol=1)) - 1 )^2)
#
# sourceCpp("src/distance.cpp")
# dd=prepare_gen_Mahlanobis_dist_cpp(Xtrain = as.matrix(Xtrain),Xtest = as.matrix(Xtest),mcov = diag(m))
#
# D <- array(dim=c(nrow(Xtrain),nrow(Xtest),m))
#
# mcov = diag(m)
# for (i in Xtest[,.I]){ # Rewrite to Rcpp
#     dec <- chol(mcov)
#     D[,i,] <- t(forwardsolve(t(dec), t(as.matrix(Xtrain)) - unlist(Xtest[i,]) )^2)
# }
#
# D.old <- distance_cpp(as.matrix(Xtrain), as.matrix(Xtest)) # MJ: We should typically scale if using Euclidean, while it is no point when using Mahalanobis


