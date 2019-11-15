
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

####################### ONLY TOUCH THINGS IN THIS SECTION ################################
experiment = "E"
true_model <- "Linear"
fitted_model <- "Linear"
variables <- "Gaussianmix" # Gaussian, Gaussianmix, or GenHyp
notes <- "Fixed rho to 0.2, increasing distance between mixtures, mu.scale"
X_dim <- 3
source.local <- ifelse(exists("source.local"),source.local,FALSE)

rho <- 0.2
pi.G <- c(0.5,0.5)
sd_noise = 0.1
nTrain <- 2000
nTest <- 100
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

mu.scale <- ifelse(exists("mu.scale"),mu.scale,1)
mu.vec <- c(1,-0.5,1)*mu.scale #

mu.list = list(-mu.vec,mu.vec)
mat <- matrix(rho,ncol=X_dim,nrow=X_dim)
diag(mat) <- 1
Sigma.list <- list(mat,mat)

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


source("paper_scripts/paper_helper_funcs.R",local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)


source("paper_experiments/source_specifying_seed_and_filenames.R",local = source.local) # Setting random or fixed seed and filenames.

#### Sampling train and test data ---------
# Creating the XYtrain, XYtest, Xtrain and Xtest objects
source("paper_experiments/source_sampling_data.R",local = source.local)

#### Fitting the model ----------

model <- fit_model_func(XYtrain)

#### Pre computation before kernel shap ---------
# Creating the l object
source("paper_experiments/source_prepare_kernelShap.R",local = source.local)

#### Computing the various Shapley approximations  --------
source("paper_experiments/source_compute_approx_Shap_with_AICc_per_testobs.R",local = source.local) # Creating Shapley.approx object
#source("paper_experiments/source_compute_approx_Shap_no_AICc.R") # Creating Shapley.approx object

#### Computing the true Shapley values ------

source("paper_experiments/source_compute_true_Shap.R",local = source.local) # Creating the Shapley.true object

#### Comparing the true and approximate values -------------


source("paper_experiments/source_compute_results.R",local = source.local) # Creating the res.DT object

# Printing the results to the terminal
print(res.DT)

#### Write results to csv files ------------
if(!is.null(joint_csv_filename)){
    fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
}

fwrite(x = res.DT,file = paste0("paper_experiments/res/single_res/",current_csv_filename))
save(Shapley.approx,Shapley.true,file=paste0("paper_experiments/res/single_res/",current_RData_filename))


##### DONE ------------------
