
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

library(xgboost)
library(GIGrvg)
library(ghyp)

####################### ONLY TOUCH THINGS IN THIS SECTION ################################
experiment = "A"
true_model <- "Linear"
fitted_model <- "Linear"
variables <- "Gaussian" # Gaussian, Gaussianmix, or GenHyp
notes <- "All var equal contribution"
X_dim <- 3

nTrain <- 2000
nTest <- 100
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

samp_model <- function(n,X,sd_noise){
    y <- rowSums(X) + rnorm(n = n,mean=0,sd=sd_noise)
}

fit_model_func <- function(XYtrain){
    lm(y~.,data=XYtrain)
}



####################################################################################################

#### Autoset helping variables. DO NOT TOUCH ####

X_GenHyp <- (variables=="GenHyp")
(joint_csv_filename <- paste0("all_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables,".csv")) # May hardcode this to NULL for not saving to joint in testing circumstances
(initial_current_csv_filename <- paste0("current_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables))


source("paper_scripts/paper_helper_funcs.R") # Helper functions these experiments (mainly computing the true Shapley values)


source("paper_experiments/source_specifying_seed_and_filenames.R") # Setting random or fixed seed and filenames.

#### Sampling train and test data ---------
# Creating the XYtrain, XYtest, Xtrain and Xtest objects
source("paper_experiments/source_sampling_data.R")

#### Fitting the model ----------

model <- fit_model_func(XYtrain)

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
if(!is.null(joint_csv_filename)){
    fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
}

fwrite(x = res.DT,file = paste0("paper_experiments/res/single_res/",current_csv_filename))
save(Shapley.approx,Shapley.true,file=paste0("paper_experiments/res/single_res/",current_RData_filename))


##### DONE ------------------
