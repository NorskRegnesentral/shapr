
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 3 ####

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)
library(xgboost)

#### NOTE: May consider running the below procedure multiple times to get a better representation of the error when optimizing sigma.
#### May alternatively use the same training data, but new test observations every time, or consider other sampling methods

#### Should anyhow try to write up the full AICc-function in RCpp -- that may give a large performance boost because that is
#### currently what takes the most time.



####################### ONLY TOUCH THINGS IN THIS SECTION ################################
joint_csv_filename <- "all_results_dim3.csv" # Set to NULL if results should not be included in any joint results table

initial_current_csv_filename <- "Experiment_3_PiecewiseConstant_XGBoost_Gaussianmix"
true_model <- "PiecewiseConstant"
fitted_model <- "XGBoost"
variables <- "Gaussianmix"
notes <- "rho set to 0.7"


rho <- 0.7#ifelse(exists("rho"),rho,0.5)
pi.G <- 1
sd_noise = 0.1
nTrain <- 2000
nTest <- 1000
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

mu.list = list(c(0,0,0),c(10,-5,10))
Sigma.list <- list(matrix(c(1,rho,rho,
                            rho,1,rho,
                            rho,rho,1),ncol=3),
                   matrix(c(1,rho,rho,
                            rho,1,rho,
                            rho,rho,1),ncol=3))

this.seed <- 123
#### Defining the true distribution of the variables and the model

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd_noise){
    y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,2]<2) + (X[,2]>4)*1 + (X[,3]<10)*1 + (X[,3]<0)*1 + (X[,1]>-5)*(X[,2]<4)*1+ rnorm(n = n,mean=0,sd=sd_noise)
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


####################################################################################################

current_csv_filename = paste0(initial_current_csv_filename,"___",stri_rand_strings(n = 1,length = 5),".csv")

source("paper_scripts/paper_helper_funcs.R") # Helper functions these experiments (mainly computing the true Shapley values)


set.seed(this.seed)


#### Sampling train and test data ---------
# Creating the XYtrain, XYtest, Xtrain and Xtest objects
source("paper_experiments/source_sampling_data.R")

#### Fitting the model ----------

model <- fit_model_func(XYtrain)

#### Pre computation before kernel shap ---------
# Creating the l object
source("paper_experiments/source_prepare_kernelShap.R")

#### Computing the various Shapley approximations  --------

source("paper_experiments/source_compute_approx_Shap_no_AICc.R") # Creating Shapley.approx object

#### Computing the true Shapley values ------

source("paper_experiments/source_compute_true_Shap.R") # Creating the Shapley.true object

#### Comparing the true and approximate values -------------


source("paper_experiments/source_compute_results.R") # Creating the res.DT object

# Printing the results to the terminal
print(res.DT)
print(res.DT)

#### Write results to csv files ------------
fwrite(x = res.DT,file = paste0("paper_experiments/res/",current_csv_filename))

if(!is.null(joint_csv_filename)){
    fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
}


##### DONE ------------------
