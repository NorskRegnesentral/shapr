### Implementing AICc in the main package


### Help functions to move into shapley.R afterwords

k.func <- function(x){
    dnorm(x = x,mean = 0,sd = 1)
}

# h.vec is vector of length q=ncol(X)
H.func <- function(h.vec,X){
    n <- nrow(X)

    H <- matrix(NA,ncol=n,nrow=n)

    for (i in 1:n){
        for (j in 1:n){
            H[i,j] <- prod(k.func((X[i,]-X[j,])/h.vec)/h.vec)
        }
        H[i,] <- H[i,]/sum(H[i,])
    }
    return(H)
}

sigma.hat.sq.func <- function(y,H){
    n <- length(y)

    sigma.hat.sq <- as.numeric(t(y)%*%t(diag(n)-H)%*%(diag(n)-H)%*%y)/n

    return(sigma.hat.sq)
}

AICc.func <- function(h.vec,y,X,negative = FALSE){
    n <- length(y)
    q <- ncol(X)

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }

    H <- H.func(h.vec = h.vec,X = X)

    sigma.hat.sq <- sigma.hat.sq.func(y=y,
                                      H = H)

    tr.H <- sum(diag(H))
    correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n)

    AICc <- log(sigma.hat.sq) + correction.term
    if(negative){
        AICc <- -AICc
    }
    return(AICc)
}


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
Sigma.list <- list(matrix(c(1,0,0,
                            0,1,0,
                            0,0,1),ncol=3))
pi.G <- 1

sd = 0.1

nTrain <- 2000
nTest <- 100


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

#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

verbose = FALSE
#,copula=5:6,Gaussian=7:8)

kernel_metric = "Gaussian"
mu = Sigma = NULL
sigma = 0.1

# Settings #

cond_approach <- list(empirical = 1:4,copula=5:6,Gaussian=7:8)
empirical_settings = list(type = "AICc_full", # May in the future allow a vector of length nrow(S) here as well to specify fixed for some and optimiziation for others
                          fixed_sigma_vec = 0.1, # Should allow this to be a vector of size nrow(S) as well
                          AICc_optimize_every_testobs = F,
                          AICc_no_samp_per_optim = 100,
                          AIC_optim_func = "nlminb",
                          AIC_optim_max_eval = 20,
                          AIC_optim_startval = 0.1)

### TODO for AICc-implementation
# 0. Define how to do things
# 1. Remove sigma from compute_kernelShap
# 2. Define default parameters
# 3. Implement checks that things are defined correctly when they are needed etc.


### TODO for combined conditional approach:
# 2. Implement check that all S-rows are included in the cond_approach list
# 3. Implement functionality that sets the  non-defined S-rows to empirical.
# 4. Implement check that l$D is not null when empirical is used and than throw an error.
# 5. Think over how to allow differnet sigmas to be used. Could let sigma be a vector or length equal to nrow(S) as well? -- only relevant when AICc is implemented

Shapley.approx <- list()

Shapley.approx$test = compute_kernelShap(model = model,
                                         l,
                                         sigma = 0.1, # Ignored when Gaussian==T
                                         w_threshold = w_threshold,
                                         n_threshold = n_threshold,
                                         verbose = FALSE,
                                         cond_approach = cond_approach,
                                         pred_zero=pred_zero,
                                         kernel_metric = "Gaussian")





