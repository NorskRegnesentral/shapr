
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
joint_csv_filename <- NULL # Set to NULL if results should not be included in any joint results table

initial_current_csv_filename <- "Testing_sensitivity_to_AICc_sampling_Gauss"
true_model <- "Linear"
fitted_model <- "Linear"
variables <- "Gaussian"
notes <- "Testing sensitivity to AICc-sampling"

rho <- 0.5
pi.G <- 1
sd_noise = 0.1
nTrain <- 2000
nTest <- 10
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,rho,rho,
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
    y <- X[,1] + X[,2] + X[,3] + rnorm(n = n,mean=0,sd=sd_noise)
}

fit_model_func <- function(XYtrain){
    lm(y~.,data=XYtrain)
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


#### Computing the various Shapley approximations ###########

empirical_fixed_sigma.01_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.1,
                                         kernel_metric = "Gaussian")

empirical_AIC_each_k_settings_10000 = list(type = "AICc_each_k",
                                           AICc_optimize_every_testobs = F,
                                           AICc_no_samp_per_optim = 10000,
                                           AIC_optim_func = "nlminb",
                                           AIC_optim_max_eval = 20,
                                           AIC_optim_startval = 0.1,
                                           kernel_metric = "Gaussian")

empirical_AIC_each_k_settings_1000 = list(type = "AICc_each_k",
                                          AICc_optimize_every_testobs = F,
                                          AICc_no_samp_per_optim = 1000,
                                          AIC_optim_func = "nlminb",
                                          AIC_optim_max_eval = 20,
                                          AIC_optim_startval = 0.1,
                                          kernel_metric = "Gaussian")


empirical_AIC_full_settings_10000 = list(type = "AICc_full",
                                         AICc_optimize_every_testobs = F,
                                         AICc_no_samp_per_optim = 10000,
                                         AIC_optim_func = "nlminb",
                                         AIC_optim_max_eval = 20,
                                         AIC_optim_startval = 0.1,
                                         kernel_metric = "Gaussian")

empirical_AIC_full_settings_1000 = list(type = "AICc_full",
                                        AICc_optimize_every_testobs = F,
                                        AICc_no_samp_per_optim = 1000,
                                        AIC_optim_func = "nlminb",
                                        AIC_optim_max_eval = 20,
                                        AIC_optim_startval = 0.1,
                                        kernel_metric = "Gaussian")


empirical_independence_settings = list(type = "independence")


#### Computing the true Shapley values ------

source("paper_experiments/source_compute_true_Shap.R") # Creating the Shapley.true object

#### Here we start the testin ############################g

### Have explored below that in this example ntrain=200 and ntest = 100, we AICc works best for small samples, when it comes to combining the similar samples
empirical_settings_1 = list(type = "AICc_full",
                            AICc_optimize_every_testobs = T,
                            AICc_no_samp_per_optim = 5000,
                            AIC_optim_func = "nlminb",
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            kernel_metric = "Gaussian")
cond_approach = "empirical"
empirical_settings = empirical_settings_1
verbose <- TRUE

mu <- Sigma <- NULL


#### Just some minor testing once agian

for (h in c(0.001,0.005,0.01,0.05,0.1,0.2,0.5)){
    summer=AICc_full_tmp_cpp(X = X_list[[1]][1:500,],
                      mcov = mcov_list[[1]],
                      S_scale_dist = T,
                      y = y_list[[1]][1:500],
                      h = h)


    print(c(summer[3],log(summer[1]/summer[3]),correction_cpp(summer[2],summer[3]),sum(c(log(summer[1]/summer[3]),correction_cpp(summer[2],summer[3])))))

}

bb=numeric()
for (aa in seq(100,2000,100)){
these = sample(1:2000,aa,replace=F)
    summer=AICc_full_tmp_cpp(X = X_list[[1]][these,],
                             mcov = mcov_list[[1]],
                             S_scale_dist = T,
                             y = y_list[[1]][these],
                             h = 0.01)
    print(aa)
    bb[aa] = summer[1]/summer[3]

}

test=rnorm(100)
test2=rnorm(1000)
test3 = rnorm(10000)
mean(test^2)
mean(test2^2)
mean(test3^2)


    print(summer[1]/summer[3])
    H = H_cpp(X = X_list[[1]][1:aa,],mcov = mcov_list[[1]],S_scale_dist = T,h = 0.01)
    org = y_list[[1]][1:aa]
    est = H%*%y_list[[1]][1:aa]

    var(org-est)

    one = diag(aa) - H
    two = one%*%y_list[[1]][1:aa]

    out = (t(two)%*%two)

    print(out/summer[3])



}
plot(bb)

    print(c(summer[3],log(summer[1]/summer[3]),correction_cpp(summer[2],summer[3]),sum(c(log(summer[1]/summer[3]),correction_cpp(summer[2],summer[3])))))

}



##############


Shapley.approx = list()

empirical_settings_1 = list(type = "AICc_full",
                            AICc_optimize_every_testobs = F,
                            AICc_no_samp_per_optim = 100,
                            AIC_optim_func = "nlminb",
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            kernel_metric = "Gaussian")

Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                l = l,
                                                                w_threshold = w_threshold,
                                                                n_threshold = n_threshold,
                                                                cond_approach = "empirical",
                                                                empirical_settings = empirical_settings_1,
                                                                pred_zero=pred_zero,
                                                                verbose = T)


empirical_settings_2 = list(type = "AICc_full",
                            AICc_optimize_every_testobs = F,
                            AICc_no_samp_per_optim = 500,
                            AIC_optim_func = "nlminb",
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            kernel_metric = "Gaussian")

Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                l = l,
                                                                w_threshold = w_threshold,
                                                                n_threshold = n_threshold,
                                                                cond_approach = "empirical",
                                                                empirical_settings = empirical_settings_2,
                                                                pred_zero=pred_zero,
                                                                verbose = T)


empirical_settings_3 = list(type = "AICc_full",
                            AICc_optimize_every_testobs = F,
                            AICc_no_samp_per_optim = 1000,
                            AIC_optim_func = "nlminb",
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            kernel_metric = "Gaussian")

Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                l = l,
                                                                w_threshold = w_threshold,
                                                                n_threshold = n_threshold,
                                                                cond_approach = "empirical",
                                                                empirical_settings = empirical_settings_3,
                                                                pred_zero=pred_zero,
                                                                verbose = T)

empirical_settings_4 = list(type = "AICc_full",
                            AICc_optimize_every_testobs = F,
                            AICc_no_samp_per_optim = 5000,
                            AIC_optim_func = "nlminb",
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            kernel_metric = "Gaussian")

Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                l = l,
                                                                w_threshold = w_threshold,
                                                                n_threshold = n_threshold,
                                                                cond_approach = "empirical",
                                                                empirical_settings = empirical_settings_4,
                                                                pred_zero=pred_zero,
                                                                verbose = T)


empirical_settings_5 = list(type = "AICc_full",
                            AICc_optimize_every_testobs = F,
                            AICc_no_samp_per_optim = 10000,
                            AIC_optim_func = "nlminb",
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            kernel_metric = "Gaussian")

Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                l = l,
                                                                w_threshold = w_threshold,
                                                                n_threshold = n_threshold,
                                                                cond_approach = "empirical",
                                                                empirical_settings = empirical_settings_5,
                                                                pred_zero=pred_zero,
                                                                verbose = T)

names(Shapley.approx) = paste0("test ",1:5)
source("paper_experiments/source_compute_results.R") # Creating the res.DT object

print(res.DT) # Printing the results to the terminal

#### GOING INTO DETAIL ####

cond_approach = "empirical"
empirical_settings = empirical_settings_5
verbose <- TRUE

mu <- Sigma <- NULL
### Checking





################## END TESTING #################


cond_approach = "empirical"
empirical_settings = empirical_AIC_each_k_settings_10000
verbose <- TRUE

mu <- Sigma <- NULL
### Checking

AICc_full_cpp(h = empirical_settings$AIC_optim_startval,
              y = pred,
              X = Xtrain.S,
              kernel="Mahalanobis",
              scale_var=F,
              S_scale_dist = T,


)


#################################################3

#### Performing various approximations methods #####

Shapley.approx = list()
cols <- c("rn",paste0("absmean_X",1:3))



for (k in 1:10){

    Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                    l = l,
                                                                    w_threshold = w_threshold,
                                                                    n_threshold = n_threshold,
                                                                    cond_approach = "empirical",
                                                                    empirical_settings = empirical_AIC_each_k_settings_10000,
                                                                    pred_zero=pred_zero)
    names(Shapley.approx)[length(Shapley.approx)] <- paste0("empirical_AIC_each_k_10000.",k)

    ### Printing intermediate results
    source("paper_experiments/source_compute_results.R") # Creating the res.DT object
    print(res.DT[grep("empirical_AIC_each_k_10000\\.",rn),.SD,.SDcols=cols])

    Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                    l = l,
                                                                    w_threshold = w_threshold,
                                                                    n_threshold = n_threshold,
                                                                    cond_approach = "empirical",
                                                                    empirical_settings = empirical_AIC_each_k_settings_1000,
                                                                    pred_zero=pred_zero)
    names(Shapley.approx)[length(Shapley.approx)] <- paste0("empirical_AIC_each_k_1000.",k)

    ### Printing intermediate results
    source("paper_experiments/source_compute_results.R") # Creating the res.DT object
    print(res.DT[grep("empirical_AIC_each_k_1000\\.",rn),.SD,.SDcols=cols])

    Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                    l = l,
                                                                    w_threshold = w_threshold,
                                                                    n_threshold = n_threshold,
                                                                    cond_approach = "empirical",
                                                                    empirical_settings = empirical_AIC_full_settings_10000,
                                                                    pred_zero=pred_zero)
    names(Shapley.approx)[length(Shapley.approx)] <- paste0("empirical_AIC_full_10000.",k)

    ### Printing intermediate results
    source("paper_experiments/source_compute_results.R") # Creating the res.DT object
    print(res.DT[grep("empirical_AIC_full_10000\\.",rn),.SD,.SDcols=cols])

    Shapley.approx[[length(Shapley.approx)+1]] = compute_kernelShap(model = model,
                                                                    l = l,
                                                                    w_threshold = w_threshold,
                                                                    n_threshold = n_threshold,
                                                                    cond_approach = "empirical",
                                                                    empirical_settings = empirical_AIC_full_settings_1000,
                                                                    pred_zero=pred_zero)
    names(Shapley.approx)[length(Shapley.approx)] <- paste0("empirical_AIC_full_1000.",k)

    ### Printing intermediate results
    source("paper_experiments/source_compute_results.R") # Creating the res.DT object
    print(res.DT[grep("empirical_AIC_full_1000\\.",rn),.SD,.SDcols=cols])

}






Shapley.approx$empirical_sigma.01 = compute_kernelShap(model = model,
                                                       l = l,
                                                       w_threshold = w_threshold,
                                                       n_threshold = n_threshold,
                                                       cond_approach = "empirical",
                                                       empirical_settings = empirical_fixed_sigma.01_settings,
                                                       pred_zero=pred_zero)


Shapley.approx$empirical_independence = compute_kernelShap(model = model,
                                                           l = l,
                                                           w_threshold = w_threshold,
                                                           n_threshold = n_threshold,
                                                           cond_approach = "empirical",
                                                           empirical_settings = empirical_independence_settings,
                                                           pred_zero=pred_zero)

Shapley.approx$Gaussian = compute_kernelShap(model = model,
                                             l,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             cond_approach = "Gaussian",
                                             pred_zero=pred_zero)

Shapley.approx$copula = compute_kernelShap(model = model,
                                           l,
                                           w_threshold = w_threshold,
                                           n_threshold = n_threshold,
                                           cond_approach = "copula",
                                           pred_zero=pred_zero)



#### Comparing the true and approximate values -------------


source("paper_experiments/source_compute_results.R") # Creating the res.DT object

print(res.DT) # Printing the results to the terminal

cols <- c("rn",paste0("absmean_X",1:3))
res.DT[grep("empirical_AIC_each_k_1000\\.",rn),.SD,.SDcols=cols]
res.DT[grep("empirical_AIC_each_k_10000\\.",rn),.SD,.SDcols=cols]
res.DT[grep("empirical_AIC_full_1000\\.",rn),.SD,.SDcols=cols]
res.DT[grep("empirical_AIC_full_10000\\.",rn),.SD,.SDcols=cols]


#### Write results to csv files ------------
fwrite(x = res.DT,file = paste0("paper_experiments/res/",current_csv_filename))

if(!is.null(joint_csv_filename)){
    fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
}


##### DONE ------------------
