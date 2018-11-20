### Implementing AICc in the main package


### Help functions to move into shapley.R afterwords


source("scripts/AICc_helper_functions.R")

######



####################33



#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 1 ####
# Linear model with independent Gaussian features

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,0.5,0.5,
                            0.5,1,0.5,
                            0.5,0.5,1),ncol=3))
pi.G <- 1

sd = 0.1

nTrain <- 2000
nTest <- 10


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




#### Sampling train and test data ---------

set.seed(123)
XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtrain[,y:=samp_model(.N,.SD,sd=sd)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest <- data.table(samp_variables(n = nTest,
                                    pi.G = pi.G,
                                    mu.list = mu.list,
                                    Sigma.list = Sigma.list))
XYtest[,y:=samp_model(.N,.SD,sd=sd)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

#### Fitting the model ----------

model = lm(y~.,data=XYtrain)

pred_zero = XYtrain[, mean(y)]
m = ncol(Xtrain)


#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4,
    distance_metric = "Mahalanobis_scaled"
)


## Common settings
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3
verbose = FALSE
kernel_metric = "Gaussian"
mu = Sigma = NULL
cond_approach <- list(empirical = 1:5,copula=6,Gaussian=7:8)

# new approach #
empirical_settings = list(type = "fixed_sigma", # May in the future allow a vector of length nrow(S) here as well to specify fixed for some and optimiziation for others
                          fixed_sigma_vec = 0.1, # Should allow this to be a vector of size nrow(S) as well
                          AICc_optimize_every_testobs = F,
                          AICc_no_samp_per_optim = 100,
                          AIC_optim_func = "nlminb",
                          AIC_optim_max_eval = 20,
                          AIC_optim_startval = 0.1)

Shapley.approx.new.1 = compute_kernelShap(model = model,
                                          l,
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = cond_approach,
                                          empirical_settings = empirical_settings,
                                          pred_zero=pred_zero,
                                          kernel_metric = "Gaussian")

empirical_settings = list(type = "AICc_each_k", # May in the future allow a vector of length nrow(S) here as well to specify fixed for some and optimiziation for others
                          fixed_sigma_vec = 0.1, # Should allow this to be a vector of size nrow(S) as well
                          AICc_optimize_every_testobs = F,
                          AICc_no_samp_per_optim = 1000,
                          AIC_optim_func = "nlminb",
                          AIC_optim_max_eval = 20,
                          AIC_optim_startval = 0.1)

Shapley.approx.new.2 = compute_kernelShap(model = model,
                                          l,
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = cond_approach,
                                          empirical_settings = empirical_settings,
                                          pred_zero=pred_zero,
                                          kernel_metric = "Gaussian")

empirical_settings = list(type = "AICc_full", # May in the future allow a vector of length nrow(S) here as well to specify fixed for some and optimiziation for others
                          fixed_sigma_vec = 0.1, # Should allow this to be a vector of size nrow(S) as well
                          AICc_optimize_every_testobs = F,
                          AICc_no_samp_per_optim = 1000,
                          AIC_optim_func = "nlminb",
                          AIC_optim_max_eval = 20,
                          AIC_optim_startval = 0.1)

Shapley.approx.new.3 = compute_kernelShap(model = model,
                                          l,
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = cond_approach,
                                          empirical_settings = empirical_settings,
                                          pred_zero=pred_zero,
                                          kernel_metric = "Gaussian")


Shapley.approx.new.1$Kshap
Shapley.approx.new.2$Kshap
Shapley.approx.new.3$Kshap

# > Shapley.approx.new.1$Kshap
# [,1]       [,2]       [,3]       [,4]
# [1,] -0.01103487 -0.8772270  1.4859423 -1.4530056
# [2,] -0.01103274  1.6807828  0.4845187 -0.8532364
# [3,] -0.01103138  0.5298590  1.4586157  1.3818112
# [4,] -0.01103635  0.1085929 -0.6472416 -2.0536210
# [5,] -0.01103589 -1.3499431 -1.3890541  0.6581233
# [6,] -0.01103285 -0.4901540  0.6086156  1.0735126
# [7,] -0.01103115  1.2402888  2.3909989 -0.3305711
# [8,] -0.01103367 -0.3713269 -0.7247810  1.5455418
# [9,] -0.01103543 -0.6397012 -0.1258849 -0.7580753
# [10,] -0.01103524 -0.4315343  0.2468977 -1.0652073
# > Shapley.approx.new.2$Kshap
# [,1]        [,2]       [,3]        [,4]
# [1,] -0.01103472 -0.92402288  1.4083895 -1.32865698
# [2,] -0.01103308  1.60172827  0.4320767 -0.72173970
# [3,] -0.01103169  0.54869436  1.4220771  1.39951451
# [4,] -0.01103589  0.06518462 -0.6954574 -1.96199709
# [5,] -0.01103546 -1.24312949 -1.3477054  0.50996094
# [6,] -0.01103311 -0.44030048  0.6354957  0.99677912
# [7,] -0.01103179  1.14766910  2.1231023  0.02994539
# [8,] -0.01103368 -0.35590079 -0.7025700  1.50790474
# [9,] -0.01103518 -0.57842659 -0.2007183 -0.74451646
# [10,] -0.01103497 -0.41377876  0.2496989 -1.08576419
# > Shapley.approx.new.3$Kshap
# [,1]        [,2]       [,3]        [,4]
# [1,] -0.01103480 -0.90466981  1.4180294 -1.35764994
# [2,] -0.01103304  1.60041317  0.4143631 -0.70271104
# [3,] -0.01103163  0.52276521  1.3755335  1.47198726
# [4,] -0.01103601  0.08269958 -0.6394314 -2.03553795
# [5,] -0.01103548 -1.26520892 -1.3024941  0.48682913
# [6,] -0.01103308 -0.46774743  0.6459795  1.01374223
# [7,] -0.01103182  1.15652151  2.0569860  0.08720926
# [8,] -0.01103362 -0.35810195 -0.7209332  1.52846900
# [9,] -0.01103522 -0.59743338 -0.1589781 -0.76724984
# [10,] -0.01103504 -0.37502151  0.2474323 -1.12225480

Shapley.approx.new.1$other_objects$h_optim_vec
Shapley.approx.new.2$other_objects$h_optim_vec
Shapley.approx.new.3$other_objects$h_optim_vec



## Old approach
sigma = 0.1

Shapley.approx.old = compute_kernelShap(model = model,
                                        l,
                                        sigma = sigma,
                                        w_threshold = w_threshold,
                                        n_threshold = n_threshold,
                                        verbose = FALSE,
                                        cond_approach = cond_approach,
                                        pred_zero=pred_zero,
                                        kernel_metric = "Gaussian")


### TODO for AICc-implementation
# 1. Implement optimization of every testobservation as well, not just sampled over all of them.
# 2. Define default parameters
# 3. Implement checks that things are defined correctly when they are needed etc.
# 4. Implement a seed for sampling the data used in the optimization such that we don't get new results every time
# 5. Consider implementing fixed cond_samp version, enough randomized already.



### TODO for combined conditional approach:
# 2. Implement check that all S-rows are included in the cond_approach list
# 3. Implement functionality that sets the  non-defined S-rows to empirical.
# 4. Implement check that l$D is not null when empirical is used and than throw an error.
# 5. Think over how to allow differnet sigmas to be used. Could let sigma be a vector or length equal to nrow(S) as well? -- only relevant when AICc is implemented

## new approach









