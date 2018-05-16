

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 1 ####

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(xgboost)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0),c(0,0,0))
Sigma.list <- list(matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3),
                   matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3))
pi.G <- c(0.5,0.5)

sd = 0.1

nTrain <- 10000
nTest <- 100


#### Defining the true distribution of the variables and the model------

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd){
    y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,2]<2) + (X[,2]>4)*1 + (X[,3]<10)*1 + (X[,3]<0)*1 + (X[,1]>-5)*(X[,2]<4)*1+ rnorm(n = n,mean=0,sd=sd)
    return(y)
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

xgb.train <- xgb.DMatrix(data = as.matrix(Xtrain),
                         label = XYtrain$y)
xgb.test <- xgb.DMatrix(data = as.matrix(Xtest),
                         label = XYtest$y)

params <- list(eta =  0.3,
               objective = "reg:linear",
               eval_metric = "rmse",
               tree_method="hist") # gpu_hist

model <- xgb.train(data = xgb.train,
                     params = params,
                     nrounds = 40,
                     print_every_n = 10,
                     ntread = 3,
                     watchlist = list(train=xgb.train,test=xgb.test))

pred_zero = XYtrain[, mean(y)]
m = ncol(Xtrain)


#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4
)

#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

Shapley.approx = list()

Shapley.approx$sigma.01 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.1,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = F,
                                             pred_zero=pred_zero)

Shapley.approx$sigma.03 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.3,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = F,
                                             pred_zero=pred_zero)

Shapley.approx$indep = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # sigma==0 gives the special case of independence (NOTE: NOT the same as setting sigma= 10^10)
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          gaussian_sample = F,
                                          pred_zero=pred_zero)

Shapley.approx$Gauss = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # Ignored when Gaussian==T
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          gaussian_sample = T,
                                          pred_zero=pred_zero)

Shapley.true = Shapley_true(model = model,
                            Xtrain = Xtrain,
                            Xtest = Xtest,
                            pi.G = pi.G,
                            mu.list = mu.list,
                            Sigma.list = Sigma.list,
                            int.samp=200,
                            l,
                            pred_zero = pred_zero)

#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true[,-1]-Shapley.approx$Gauss$Kshap[,-1])))

# Mean of the absolute errors over all variables
mean(absmeans.sigma.01)
mean(absmeans.sigma.03)
mean(absmeans.indep)
mean(absmeans.Gauss)

# Insert ranking based measures etc. here as well.

