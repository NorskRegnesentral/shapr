
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 2 ####

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

#### NOTE: May consider running the below procedure multiple times to get a better representation of the error when optimizing sigma.
#### May alternatively use the same training data, but new test observations every time, or consider other sampling methods

#### Should anyhow try to write up the full AICc-function in RCpp -- that may give a large performance boost because that is
#### currently what takes the most time.



####################### ONLY TOUCH THINGS IN THIS SECTION ################################
joint_csv_filename <- "all_results_dim3.csv" # Set to NULL if results should not be included in any joint results table

initial_current_csv_filename <- "Experiment_2_Linear_Linear_Gaussianmix"
true_model <- "Linear"
fitted_model <- "Linear"
variables <- "Gaussianmix"
notes <- "rho set to 0.7"


rho <- 0.7#ifelse(exists("rho"),rho,0)
pi.G <- c(0.5,0.5)
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

#### Defining the true distribution of the variables and the model

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd_noise){
    y <- X[,1] + X[,2] + X[,3] + rnorm(n = n,mean=0,sd=sd_noise)
}

fit_model_func <- function(XYtrain){
    lm(y~.,data=XYtrain)
}


####################################################################################################
seed0 <- print(as.numeric(Sys.time())*1000,digits=10) # Getting a time based seed.
(this.seed <- abs(seed0 - signif(seed0)))

run_indicator <- stri_rand_strings(n = 1,length = 5)
run_date_time <- Sys.time()

current_csv_filename = paste0(initial_current_csv_filename,"___",run_indicator,".csv")
current_RData_filename = paste0(initial_current_csv_filename,"___",run_indicator,".RData")

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

save(Shapley.approx,Shapley.true,file=paste0("paper_experiments/res/",current_RData_filename))

##### DONE ------------------
