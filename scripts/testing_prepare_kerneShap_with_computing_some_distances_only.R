#### TESTING DIFFERNET VERSIONS OF THE KERNELSHAP METHOD #####

rm(list=ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

library(xgboost)
library(GIGrvg)
library(ghyp)

####################### ONLY TOUCH THINGS IN THIS SECTION ################################
X_dim <- 6 # use 3,6,9,12,15
source.local <- ifelse(exists("source.local"),source.local,FALSE)

nTrain <- 200
nTest <- 3
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

pi.G <- 1
sd_noise = 0.1
rho <- ifelse(exists("rho"),rho,0.5) # Do not edit
mu.list = list(rep(0,X_dim))
mat <- matrix(rho,ncol=X_dim,nrow=X_dim)
diag(mat) <- 1
Sigma.list <- list(mat)

#### Defining the true distribution of the variables and the model

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd_noise,X_dim){
    y <- stepwiseConstant_fun1(X[,1])*1 + stepwiseConstant_fun2(X[,2])*1 + stepwiseConstant_fun3(X[,3])*1  + rnorm(n = n,mean=0,sd=sd_noise)
        if(X_dim>=6){
        y <- y + stepwiseConstant_fun1(X[,4])*1 + stepwiseConstant_fun2(X[,5])*1 + stepwiseConstant_fun3(X[,6])*1
        }
    if(X_dim>=9){
        y <- y + stepwiseConstant_fun1(X[,7])*1 + stepwiseConstant_fun2(X[,8])*1 + stepwiseConstant_fun3(X[,9])*1
    }
    if(X_dim>=12){
        y <- y + stepwiseConstant_fun1(X[,10])*1 + stepwiseConstant_fun2(X[,11])*1 + stepwiseConstant_fun3(X[,12])*1
    }
    if(X_dim>=15){
        y <- y + stepwiseConstant_fun1(X[,13])*1 + stepwiseConstant_fun2(X[,14])*1 + stepwiseConstant_fun3(X[,15])*1
    }

    return(y)

}

fit_model_func <- function(XYtrain){
    xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[,-"y"]),
                             label = XYtrain[,y])

    params <- list(eta =  0.3,
                   objective = "reg:linear",
                   eval_metric = "rmse",
                   tree_method="hist") # gpu_hist

    model <- xgb.train(data = xgb.train,
                       params = params,
                       nrounds = 50,
                       print_every_n = 10,
                       ntread = 3)
    return(model)
}



source("paper_scripts/paper_helper_funcs.R",local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)

set.seed(123)

#### Sampling train and test data ---------
XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtest <- data.table(samp_variables(n = nTest,
                                    pi.G = pi.G,
                                    mu.list = mu.list,
                                    Sigma.list = Sigma.list))

XYtrain[,y:=samp_model(.N,.SD,sd_noise=sd_noise,X_dim=X_dim)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest[,y:=samp_model(.N,.SD,sd_noise=sd_noise,X_dim=X_dim)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

pred_zero = XYtrain[, mean(y)]

#### Fitting the model ----------

model <- fit_model_func(XYtrain)

#### Pre computation before kernel shap ---------
# Creating the l object
m = X_dim
exact = TRUE
replace = FALSE
nrows = NULL
shapley_weight_inf_replacement = 10^6
scale = FALSE
use_shapley_weights_in_W = T
normalize_W_weights = T
distance_metric = "Mahalanobis_scaled"
compute_distances_for_these_varcomb = c(62:64,3:10)
normalize_distance_rows = TRUE


l <- prepare_kernelShap(m = m,
                        Xtrain = Xtrain,
                        Xtest = Xtest,
                        exact = exact,
                        replace = replace,
                        nrows = nrows,
                        shapley_weight_inf_replacement = shapley_weight_inf_replacement,
                        scale = scale,
                        use_shapley_weights_in_W = use_shapley_weights_in_W,
                        normalize_W_weights = normalize_W_weights,
                        distance_metric = distance_metric,
                        compute_distances_for_these_varcomb = compute_distances_for_these_varcomb,
                        normalize_distance_rows = normalize_distance_rows)

empirical_fixed_sigma.01_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.1,
                                         kernel_metric = "Gaussian")

#Shapley.approx <- list()


mu = NULL
Sigma = NULL
cond_approach = list(empirical = 6:9,Gaussian = c(1:5,10:64))
empirical_settings = empirical_fixed_sigma.01_settings
verbose = T


Shapley.approx$comb_sigma.01 = compute_kernelShap(model = model,
                                                  l = l,
                                                  w_threshold = w_threshold,
                                                  n_threshold = n_threshold,
                                                  cond_approach = cond_approach,
                                                  empirical_settings = empirical_settings,
                                                  pred_zero=pred_zero,
                                                  mu = mu,
                                                  Sigma = Sigma)

empirical_settings = list(type = "AICc_full",
                          fixed_sigma_vec = 0.1,
                          AICc_no_samp_per_optim = 1000,
                          AICc_optimize_every_testobs = T,
                          AIC_optim_func = "nlminb", # only "nlminb" allowed for now
                          AIC_optim_max_eval = 20,
                          AIC_optim_startval = 0.1,
                          AICc_combination_type = "alternative", # "alternative" or "standard" # "Standard" combines sds and partial H's before computing one AICc, while "alternative computes AICc seperatedly and combines them.
                          kernel_metric = "Gaussian",
                          AICc_force_use_all_trainsamp_per_optim = T)



