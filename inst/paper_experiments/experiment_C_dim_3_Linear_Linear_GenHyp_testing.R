
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

tot.DT.list <- list()

beta.scale.vec <- c(1,5,10)
this.seed=123
for (beta.scale in beta.scale.vec){



####################### ONLY TOUCH THINGS IN THIS SECTION ################################
experiment = "C"
true_model <- "Linear"
fitted_model <- "Linear"
variables <- "GenHyp" # Gaussian, Gaussianmix, or GenHyp
notes <- "All var equal contribution. Increasing beat.scale, while adjusting mu for constant mean, but increasing variance and skewness."
X_dim <- 3
source.local <- ifelse(exists("source.local"),source.local,FALSE)

sd_noise = 0.1
nTrain <- 10000
nTest <- 100
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^4 # Number of samples used in the Monte Carlo integration

beta.scale <- ifelse(exists("beta.scale"),beta.scale,1)
beta <- c(0.25,0.25,0.25)*beta.scale
EW = 4.56

rho <- 0
lambda <- 1
Sigma <- matrix(rho,ncol=X_dim,nrow=X_dim)
diag(Sigma) <- rep(1,3)

omega <- 0.5
mu     <- rep(0,3) - EW*mean(beta)



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

#### Computing the various Shapley approximations ###########

empirical_fixed_sigma.01_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.1,
                                         kernel_metric = "Gaussian")

empirical_fixed_sigma.settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.2,
                                         kernel_metric = "Gaussian")



empirical_independence_settings = list(type = "independence")


#### Performing various approximations methods #####

Shapley.approx = list()


vec <- seq(0.02,0.4,by=0.02)
for(k in 1:length(vec)){
    Shapley.approx[[k]] = compute_kernelShap(model = model,
                                             l = l,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             cond_approach = "empirical",
                                             empirical_settings = list(type = "fixed_sigma",
                                                                       fixed_sigma_vec = vec[k],
                                                                       kernel_metric = "Gaussian"),
                                             pred_zero=pred_zero)
    names(Shapley.approx)[k] <- paste0("empirical_sigma.",vec[k])
    print(k)
}



Shapley.approx$empirical_independence = compute_kernelShap(model = model,
                                                           l = l,
                                                           w_threshold = w_threshold,
                                                           n_threshold = n_threshold,
                                                           cond_approach = "empirical",
                                                           empirical_settings = empirical_independence_settings,
                                                           pred_zero=pred_zero)

Shapley.approx$Gaussian = compute_kernelShap(model = model,
                                             l = l,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             cond_approach = "Gaussian",
                                             pred_zero=pred_zero)

Shapley.approx$copula = compute_kernelShap(model = model,
                                           l = l,
                                           w_threshold = w_threshold,
                                           n_threshold = n_threshold,
                                           cond_approach = "copula",
                                           pred_zero=pred_zero)







#source("paper_experiments/source_compute_approx_Shap_no_AICc.R") # Creating Shapley.approx object

#### Computing the true Shapley values ------

source("paper_experiments/source_compute_true_Shap.R",local = source.local) # Creating the Shapley.true object

#### Comparing the true and approximate values -------------


source("paper_experiments/source_compute_results.R",local = source.local) # Creating the res.DT object

# Printing the results to the terminal
#print(res.DT)

##### Write results to csv files ------------
#if(!is.null(joint_csv_filename)){
#    fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
#}
#
#fwrite(x = res.DT,file = paste0("paper_experiments/res/single_res/",current_csv_filename))
#save(Shapley.approx,Shapley.true,file=paste0("paper_experiments/res/single_res/",current_RData_filename))
#

##### DONE ------------------
tot.DT.list[[length(tot.DT.list)+1]] <- copy(res.DT)
}

tot.DT <- rbindlist(tot.DT.list)

setkey(tot.DT,beta.scale,rn,absmean_total)

tot.DT[rn %in% c("Gaussian","copula","empirical_sigma.0.1"),.(rn,absmean_total,beta.scale)]

# rn absmean_total beta.scale
# 1:            Gaussian    0.26308074          1
# 2:              copula    0.30196033          1
# 3: empirical_sigma.0.1    0.08449379          1
# 4:            Gaussian    0.12780794          5
# 5:              copula    0.29389499          5
# 6: empirical_sigma.0.1    0.12827104          5
# 7:            Gaussian    0.06159195         10
# 8:              copula    0.13156494         10
# 9: empirical_sigma.0.1    0.14848129         10

