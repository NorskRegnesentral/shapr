
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 5 ####

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)
library(xgboost)
library(GIGrvg)
library(ghyp)

#### NOTE: May consider running the below procedure multiple times to get a better representation of the error when optimizing sigma.
#### May alternatively use the same training data, but new test observations every time, or consider other sampling methods

####################### ONLY TOUCH THINGS IN THIS SECTION ################################
joint_csv_filename <- "all_results_GenHyp10dim.csv" # Set to NULL if results should not be included in any joint results table

initial_current_csv_filename <- "Experiment_5_PiecewiseConstant_XGBoost_GenHyp10dim_heavytails"
true_model <- "PiecewiseConstant"
fitted_model <- "XGBoost"
variables <- "GenHyp10dim_heavytails"
notes <- "10 dimensional"
X_GenHyp <- TRUE


# Setting X-distribution parameters
lambda <- 1
mu     <- c(1:3,1:3,1:3,3)-2
Sigma <- diag(c(1:3,1:3,1:3,3))
beta <- c(rep(1,5),rep(0.5,5))
omega <- 0.5

sd_noise = 0.1
nTrain <- 2000#2000
nTest <- 100#1000
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

#### Defining the true distribution of the variables and the model

samp_variables <- function(n,Sigma,beta,omega,lambda,mu){

    X <- simulateGenHyperbolic(nSim=n,
                               Sigma=Sigma,
                               beta = beta,
                               omega = omega,
                               lambda = lambda,
                               mu = mu)
    return(X)
}

samp_model <- function(n,X,sd_noise){
#    y <- 0.1*X[,2]  +  (X[,1]<0)*1 + (X[,2]>-1)*1 - (X[,3]<1)*1 + (X[,3]<-1)*4 - (X[,3]>-1)*(X[,2]<-1)*2+ rnorm(n = n,mean=0,sd=sd_noise)
#    y <- (0.1*X[,2]  + (X[,1]<2)*3 + (X[,3]<2)*1.5 + (X[,4]>4)*1 + (X[,5]<6)*1 + (X[,6]<0)*1.5 + (X[,7]>0)*1.5 + (X[,7]>-2)*(X[,8]<4)*1+ 0.1*X[,9] + 0.1*X[,10])+ rnorm(n, mean=0, sd = sd_noise)
    y <- (0.05*X[,2]  + (X[,1]<0)*1 + (X[,3]<0)*1.5 + (X[,4]>2)*1 + (X[,5]<4)*1 + (X[,6]<1)*0.5 + (X[,7]>-2)*1.5 + (X[,7]>-4)*(X[,8]<3)*0.5 + 0.05*X[,9] + 0.05*X[,10])+ rnorm(n, mean=0, sd = sd_noise)
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
                       ntread = 10)
    return(model)
}


####################################################################################################
source("paper_scripts/paper_helper_funcs.R") # Helper functions these experiments (mainly computing the true Shapley values)

source("paper_experiments/source_specifying_seed_and_filenames.R") # Setting random or fixed seed and filenames.

#### Sampling train and test data ---------
# Creating the XYtrain, XYtest, Xtrain and Xtest objects
source("paper_experiments/source_sampling_data.R")

#### Fitting the model ----------

model <- fit_model_func(XYtrain)

#xgb.importance(model=model)
#summary(XYtrain)

#### Pre computation before kernel shap ---------
# Creating the l object
source("paper_experiments/source_prepare_kernelShap.R")

#### Computing the various Shapley approximations  --------
source("paper_experiments/source_compute_approx_Shap_with_AICc_per_testobs.R") # Creating Shapley.approx object
#source("paper_experiments/source_compute_approx_Shap_no_AICc.R") # Creating Shapley.approx object

#### Computing the true Shapley values ------

source("paper_experiments/source_compute_true_Shap.R") # Creating the Shapley.true object

#### Comparing the true and approximate values -------------


source("paper_experiments/source_compute_results.R") # Creating the res.DT object

# Printing the results to the terminal
print(res.DT)

#### Write results to csv files ------------
fwrite(x = res.DT,file = paste0("paper_experiments/res/",current_csv_filename))

if(!is.null(joint_csv_filename)){
    fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
}

save(Shapley.approx,Shapley.true,file=paste0("paper_experiments/res/",current_RData_filename))


##### DONE ------------------
