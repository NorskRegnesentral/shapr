## For Martin
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)
library(xgboost)
library(ghyp)
library(party)


rm(list = ls())
setwd("M:/BigInsight/Projects/Explanations/Annabelle/shapr/")

##
experiment = "D"
true_model <- "PiecewiseConstant"
fitted_model <- "XGBoost"
variables <- "GenHyp" # Gaussian, Gaussianmix, or GenHyp
notes <- ""
X_dim <- 6 # 12 # 15 ## this was changed
source.local <- ifelse(exists("source.local"), source.local,FALSE)

sd_noise = 0.1
nTrain <- 2000 # 2000 # 50
nTest <- 100 # 100 # 5
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

rho <- 0

if(X_dim == 3){
    beta.scale <-  ifelse(exists("beta.scale"),beta.scale,1)
    beta <- c(0.25, 0.25, 0.25)*beta.scale
    lambda <- 1
    EW = 4.56
    rho <- 0
    lambda <- 1
    Sigma <- matrix(rho, ncol = X_dim, nrow = X_dim)
    diag(Sigma) <- rep(1,3)
    omega <- 0.5
    mu <- rep(0,3) - EW*mean(beta)

}

if(X_dim == 6){
    beta.scale <-  ifelse(exists("beta.scale"),beta.scale,1)
    lambda <- 1
    mu <- rep(3,6)
    Sigma <- diag(c(1:3,1:3))
    beta <- c(rep(1,3),rep(0.5,3))
    omega <- 0.5
}

if(X_dim == 10){
    beta.scale <-  ifelse(exists("beta.scale"),beta.scale,1)
    lambda <- 1
    mu <- rep(3,10)
    Sigma <- diag(c(1:3,1:3,1:3,3))
    beta <- c(rep(1,5),rep(0.5,5))
    omega <- 0.5
}

fixed_sigma_vec <- 0.1

#### Defining the true distribution of the variables and the model
samp_variables <- function(n,Sigma,beta,omega,lambda,mu){

    X <- simulateGenHyperbolic(nSim=n,
                               Sigma=Sigma,
                               beta=beta,
                               omega=omega,
                               lambda=lambda,
                               mu=mu)
    return(X)
}

samp_model <- function(n,X,sd_noise){ ## this was changed
    y <- stepwiseConstant_fun1(X[,1]) + stepwiseConstant_fun1(X[,4]) + stepwiseConstant_fun2(X[,2])*1 +
        stepwiseConstant_fun2(X[,5])*1 + stepwiseConstant_fun3(X[,3])*1 +
        rnorm(n = n,mean=0,sd=sd_noise)
}

# samp_model <- function(n,X,sd_noise){ ## this was changed
#     y <- stepwiseConstant_fun1(X[,1]) + stepwiseConstant_fun2(X[,2])*1 + stepwiseConstant_fun3(X[,3])*1 + rnorm(n = n,mean=0,sd=sd_noise)
# }



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

##
X_GenHyp <- (variables=="GenHyp")
(joint_csv_filename <- paste0("all_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables,".csv")) # May hardcode this to NULL for not saving to joint in testing circumstances
(initial_current_csv_filename <- paste0("current_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables))

source("paper_scripts/paper_helper_funcs.R", local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)

# source("paper_experiments/source_specifying_seed_and_filenames.R", local = source.local) # Setting random or fixed seed and filenames.

# -------------- 10 dimensional April seed ---------------------------- #
this.seed <- 2693752
set.seed(this.seed)
current_RData_filename = "current_results_experiment_D_dim_6_PiecewiseConstant_XGBoost_GenHyp__runind_hhEGo__6_features__this.seed_2693752.RData"
# current_RData_filename = "current_results_experiment_D_dim_3_PiecewiseConstant_XGBoost_GenHyp__runind_hhEGo__3_features__this.seed_2693752.RData"


#### Sampling train and test data ---------
source("paper_experiments/source_sampling_data.R", local = source.local) # Creating the XYtrain, XYtest, Xtrain and Xtest objects

#### Fitting the model ----------
model <- fit_model_func(XYtrain) ## fits xgboost model

#### Pre computation before kernel shap ---------
source("paper_experiments/source_prepare_kernelShap.R", local = source.local) # Creating the l object

#### Computing the various Shapley approximations  --------
source("paper_experiments/source_compute_approx_Shap_no_AICc.R", local = source.local) # Creating Shapley.approx object -- this takes a while to run

#### Computing the true Shapley values ------
source("paper_experiments/source_compute_true_Shap.R",local = source.local) # Creating the Shapley.true object

#### Comparing the true and approximate values -------------
source("paper_experiments/source_compute_results.R",local = source.local) # Creating the res.DT object
res.DT[,.(rn,skillscoremean_total, absmean_total)]

# #### Write results to RData files ------------
# save(Shapley.approx, Shapley.true, res.DT, file = paste0("Annabelle/res/Dim_6/May/6_features/", current_RData_filename))
