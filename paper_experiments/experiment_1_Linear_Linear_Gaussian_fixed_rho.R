
#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 1 ####
# Linear true model
# Linear fitted model
# Gaussian variables
# Fixed rho

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

####################### ONLY TOUCH THINGS IN THIS SECTION ################################

rho <- 0
pi.G <- 1
sd = 0.1
nTrain <- 200# 2000
nTest <- 100 # 1000
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,rho,rho,
                            rho,1,rho,
                            rho,rho,1),ncol=3))

this.seed <- 123
#### Defining the true distribution of the variables and the model------

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd){
    y <- X[,1] + X[,2] + X[,3] + rnorm(n = n,mean=0,sd=sd)
}

fit_model_func <- function(XYtrain){
    lm(y~.,data=XYtrain)
}


####################################################################################################

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

source("paper_experiments/source_compute_approx_Shap.R")

#### Computing the true Shapley values ------


source("paper_experiments/source_compute_true_Shap.R")

#### Running AICc to optimize the sigma in the empirical version ####

source("scripts/AICc_helper_functions.R")

# Computing for the first test observation only


#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
(absmeans.sigma.AICc = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.AICc$Kshap[,-1])))


# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),E_KS_AICc=mean(absmeans.sigma.AICc),TreeSHAP = NA)
spec <- data.frame(gx = "Linear", fx="Linear",px="Gaussian",rho=0)
res_to_paper <- cbind(spec,res_to_paper)
#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#0.01789263 0.01903016 0.03861126 0.02175289

# Insert ranking based measures etc. here as well.

