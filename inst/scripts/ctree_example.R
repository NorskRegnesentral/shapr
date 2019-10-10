# library(shapr)
# library(data.table)
# library(mvtnorm)
# library(condMVNorm)
# library(stringi)
# library(xgboost)
# library(ghyp)
# library(party)


rm(list = ls())
# setwd("M:/BigInsight/Projects/Explanations/Annabelle/shapr/")

### Defining sampling data and model
dimX <- 5
rho <- 0.5
nTrain <- 500
nTest <- 10

mu <- 1:dimX
Sigma <- matrix(rho, ncol = dimX, nrow = dimX)
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

XYtrain[, y := samp_model(.N, .SD, sd_noise = sd_noise)]
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

l <- prepare_kshap(
  Xtrain = Xtrain,
  Xtest = Xtest)

Shapley.approx = list()

# Empirical version with sigma set to the default value of 0.1
Shapley.approx$ctree_alpha.01 = compute_kshap(model = model,
                                                  l = l,
                                              cond_approach = 'ctree',
                                                  pred_zero=pred_zero)




