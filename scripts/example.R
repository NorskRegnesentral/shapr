rm(list = ls())

library(data.table)
library(mvtnorm)
library(shapr)

### Defining sampling data and model
dimX <- 5
rho <- 0.5
nTrain <- 500
nTest <- 50

mu <- 1:dimX
Sigma <- matrix(rho,ncol=dimX,nrow=dimX)
diag(Sigma) <- 1:dimX


samp_variables <- function(n,mu,Sigma){
    data.table(rmvnorm(n = n,
                       mean = mu,
                       sigma = Sigma))

}

samp_model <- function(n,X,sd_noise){
    y <- rowSums(X) + rnorm(n = n,mean=0,sd=sd_noise)
}

fit_model_func <- function(XYtrain){
    lm(y~.,data=XYtrain)
}


sd_noise <- 0.5

#### Sampling train and test data

set.seed(123)

XYtrain <- data.table(samp_variables(n = nTrain,
                                     mu = mu,
                                     Sigma = Sigma))

XYtrain[,y:=samp_model(.N,.SD,sd_noise=sd_noise)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

# Just features for testing
Xtest <- data.table(samp_variables(n = nTest,
                                   mu = mu,
                                   Sigma = Sigma))


pred_zero = XYtrain[, mean(y)] # Storing the mean prediction

#### Fitting the model

model <- fit_model_func(XYtrain)


#### Preparing the data for kernelShap

l <- prepare_kernelShap(
    m = ncol(Xtrain),
    Xtrain = Xtrain,
    Xtest = Xtest)

#### Running a few different versions of kernelShap


Shapley.approx = list()

# Empirical version with sigma set to the default value of 0.1
Shapley.approx$empirical_sigma.01 = compute_kernelShap(model = model,
                                                       l = l,
                                                       pred_zero=pred_zero)

# Gaussian approach
Shapley.approx$Gaussian = compute_kernelShap(model = model,
                                             l = l,
                                             cond_approach = "Gaussian",
                                             pred_zero=pred_zero)

# Combined Gaussian and empirical with sigma=0.1
Shapley.approx$comb = compute_kernelShap(model = model,
                                         l = l,
                                         cond_approach = list(empirical=1:5, Gaussian=6:32),
                                         pred_zero=pred_zero)


### Just looking at some of the results

head(Shapley.approx$empirical_sigma.01$Kshap)

head(Shapley.approx$Gaussian$Kshap)

head(Shapley.approx$comb$Kshap)



