library(data.table)
library(MASS)
library(Matrix)

# (VIKTIG!) Installer spesifikk versjon av shapr-pakka som inneholder reduksjonsmetoden.
#remotes::install_github("NorskRegnesentral/shapr",ref="frida/shapley_feature_reduction")

library(shapr)
library(future)
library(xgboost)

###################################################################################################
#
# Har sjekket at naar alle korrelasjoner er 0 og vi ikke har interaksjoner, saa faar vi de
# teoretiske Shapley-verdiene
#
##################################################################################################
nVar       <- 12
nVarRed    <- 3
rhoLarge  <- 0.98
rhoSmall <- 0.0
rhoBetween <- 0.0
corMat <- diag(nVar)
corMat[1:nVarRed,1:nVarRed]                 <- rhoLarge
corMat[(nVarRed+1):nVar,(nVarRed+1):nVar]   <- rhoSmall
corMat[1:nVarRed,(nVarRed+1):nVar]          <- rhoBetween
corMat[(nVarRed+1):nVar,1:nVarRed]          <- rhoBetween
diag(corMat) <- 1
mu <- rep(0,nVar)
indep.ind <- seq(nVarRed+1,nVar)

predFuncT <- function(X)
{
  n <- dim(X)[1]
  eps <- rnorm(n,0,0.01)
  y <- 0.5*X[,1]+0.3*X[,2]+0.4*X[,3]+0.2*X[,4]+0.6*X[,5]+0.3*X[,6]+0.2*X[,7]+0.1*X[,8]
  y <- y + eps
}

#################################################################################################
###################################################################################################
nTrain  <- 10000
nTest   <- 100
simSeed <-100
set.seed(simSeed)
library(mvtnorm)
X       <- rmvnorm(n=nTrain,mean=mu,sigma=corMat)
Xtest   <- rmvnorm(n=nTest,mean=mu,sigma=corMat)

y <- predFuncT(X)
trainData <- as.data.frame(cbind(y,X))

ytest <- predFuncT(Xtest)
testData <- as.data.frame(cbind(ytest,Xtest))

model <- lm(y~., data = trainData)

###########################################################################################################

muNorm    <- apply(trainData[,-1],2,mean)
covMat    <- var(trainData[,-1])
coef <- model$coef[-1]
indTest <- 1
x0 <- as.numeric(testData[indTest,-1])

####### MJ translating to my setup ####

Sigma <- covMat
mu <- muNorm
p0 <- mean(predict(model,trainData[,-1]))

x_train <- as.data.table(X)
names(x_train) <- paste0("V",1+1:nVar)

x_explain <- as.data.table(testData[,-1])
names(x_explain) <- paste0("V",1+1:nVar)

set.seed(123)


#### SETTINGS FOR SHAPLEY VALUE COMPUTATIONS ####
allow_feature_reduction = TRUE # Set to FALSE to compute Shapley values without removing features
paired_shap_sampling = TRUE # Whether we sample coalitions in pairs or not
fixed_n_coalitions_per_iter = 10 # Number of new unique coalitions to in each iteration. Only one feature can be removed in each iteration.
# In the standard setup this is usually set based on estimated number of coalitions needed to reach convergence,
# but it makes more sense to fix it in the feature removal setting.
max_iter = 500 # Upper limit of the number of iterations (not coalitions)
initial_n_coalitions = 50 # Number of unique coalitions to sample in the first iteration
shapley_threshold_val = 0.1 # The z in the formula for when the remove feature j: Pr(|\phi_j| > z) < y
shapley_threshold_prob = 0.2 # The z in the formula for when the remove feature j: Pr(|\phi_j| > z) < y
adaptive = TRUE # Whether to compute Shapley values iteratively or not. Must be used when allow_feature_reduction = TRUE
n_MC_samples <- 1000 # Number of Monte Carlo samples used in the numerical integration of the Shapley values
approach = "gaussian" # "gaussian", "ctree", "vaeac", "independence" or similar. See ?shapr::explain for the full set of options.
gaussian.mu=mu # The mean of the Gaussian distribution used in the Gaussian approach. Set to NULL to estimate it from x_explain
gaussian.cov_mat=Sigma # The covariance matrix of the Gaussian distribution used in the Gaussian approach. Set to NULL to estimate it from x_explain

ret_list <- list()

i= 2

expl_red <- shapr::explain(model = model,
                           x_explain= x_explain[i,], # For allow_feature_reduction = TRUE, this must contain only a SINGLE row
                           x_train = x_train,
                           approach = approach,
                           n_MC_samples = n_MC_samples,
                           prediction_zero = p0,
                           gaussian.mu=gaussian.mu,
                           gaussian.cov_mat=gaussian.cov_mat,
                           adaptive = adaptive,
                           print_iter_info = TRUE,
                           paired_shap_sampling  = paired_shap_sampling,
                           adaptive_arguments = list(allow_feature_reduction = FALSE,  # Set to FALSE to run regular
                                                     fixed_n_coalitions_per_iter = fixed_n_coalitions_per_iter,
                                                     max_iter = 10,
                                                     initial_n_coalitions = initial_n_coalitions,
                                                     shapley_threshold_val = shapley_threshold_val,
                                                     shapley_threshold_prob = shapley_threshold_prob),
                           shapley_comp_method = "kernel")

expl_red_frida <- shapr::explain(model = model,
                           x_explain= x_explain[i,], # For allow_feature_reduction = TRUE, this must contain only a SINGLE row
                           x_train = x_train,
                           approach = approach,
                           n_MC_samples = n_MC_samples,
                           prediction_zero = p0,
                           gaussian.mu=gaussian.mu,
                           gaussian.cov_mat=gaussian.cov_mat,
                           adaptive = adaptive,
                           print_iter_info = TRUE,
                           paired_shap_sampling  = paired_shap_sampling,
                           adaptive_arguments = list(allow_feature_reduction = FALSE,  # Set to FALSE to run regular
                                                     fixed_n_coalitions_per_iter = fixed_n_coalitions_per_iter,
                                                     max_iter = 10,
                                                     initial_n_coalitions = initial_n_coalitions,
                                                     shapley_threshold_val = shapley_threshold_val,
                                                     shapley_threshold_prob = shapley_threshold_prob),
                           shapley_comp_method = "frida")

time_frida <- time_kernel <- 0

expl_red_both <- shapr::explain(model = model,
                                 x_explain= x_explain[i,], # For allow_feature_reduction = TRUE, this must contain only a SINGLE row
                                 x_train = x_train,
                                 approach = approach,
                                 n_MC_samples = n_MC_samples,
                                 prediction_zero = p0,
                                 gaussian.mu=gaussian.mu,
                                 gaussian.cov_mat=gaussian.cov_mat,
                                 adaptive = adaptive,
                                 print_iter_info = TRUE,
                                 paired_shap_sampling  = paired_shap_sampling,
                                 adaptive_arguments = list(allow_feature_reduction = FALSE,  # Set to FALSE to run regular
                                                           fixed_n_coalitions_per_iter = fixed_n_coalitions_per_iter,
                                                           max_iter = 3,
                                                           initial_n_coalitions = 20,
                                                           shapley_threshold_val = shapley_threshold_val,
                                                           shapley_threshold_prob = shapley_threshold_prob,
                                                           n_boot_samps = 1000),
                                 shapley_comp_method = "both")

time_frida

time_kernel


expl_red_both$internal$iter_results$dt_iter_shapley_est[,-(1:3)]

t(sapply(expl_red_both$internal$iter_list,function(x) x$frida_shapley_values))

### OK So, shapley values are the same. The bootstrap estimates are not identical after first iteration...
# First try to increase the number of iterations

expl_red_both$internal$iter_results$dt_iter_shapley_sd[,-(1:3)]


t(sapply(expl_red_both$internal$iter_list,function(x) x$frida_boot_shapley_values))


for(i in seq_len(nrow(x_explain))){

  expl_red <- shapr::explain(model = model,
                             x_explain= x_explain[i,], # For allow_feature_reduction = TRUE, this must contain only a SINGLE row
                             x_train = x_train,
                             approach = approach,
                             n_MC_samples = n_MC_samples,
                             prediction_zero = p0,
                             gaussian.mu=gaussian.mu,
                             gaussian.cov_mat=gaussian.cov_mat,
                             adaptive = adaptive,
                             print_iter_info = TRUE,
                             paired_shap_sampling  = paired_shap_sampling,
                             adaptive_arguments = list(allow_feature_reduction = allow_feature_reduction,  # Set to FALSE to run regular
                                                       fixed_n_coalitions_per_iter = fixed_n_coalitions_per_iter,
                                                       max_iter = max_iter,
                                                       initial_n_coalitions = initial_n_coalitions,
                                                       shapley_threshold_val = shapley_threshold_val,
                                                       shapley_threshold_prob = shapley_threshold_prob))


  n_iter <- length(expl_red$internal$iter_list)
  total_used_coal <- sapply(expl_red$internal$iter_list, function(sublist) sublist$total_n_coalitions)
  it_prob_of_val_above_threshold_val <- rbindlist(lapply(expl_red$internal$iter_list, function(sublist) sublist$prob_of_red)[-n_iter],fill=TRUE)

  ll <- list(total_used_coal = total_used_coal, # Total number of v(S) evaluated after every iteration
             it_shap_res = expl_red$internal$iter_results$dt_iter_shapley_est, # The estimated shapley values after every iteration
             it_shap_sd = expl_red$internal$iter_results$dt_iter_shapley_sd,# The estimated sd of the shapley values after every iteration
             it_prob_of_val_above_threshold_val = it_prob_of_val_above_threshold_val, # The probability of the shapley values being above shapley_threshold_val after every iteration
             dropped_features = expl_red$internal$iter_list[[n_iter]]$shap_reduction$dropped_features # Vector of feature being removed
  )
  ret_list[[i]] <- ll
  print(i)
}

# The final Shapley values for all observations with the reduction method
red_shap_vals <- rbindlist(lapply(ret_list, function(x) x$it_shap_res[.N]))

# The fianl number of coalitions used for each observations
tot_used_coal <- sapply(ret_list, function(x) x$total_used_coal[length(x$total_used_coal)])


# Plain iterative computation for all observations (just for illustration, not used for anything below)
expl_standard <- shapr::explain(model = model,
                                x_explain= x_explain,
                                x_train = x_train,
                                approach = approach,
                                n_MC_samples = n_MC_samples,
                                prediction_zero = p0,
                                gaussian.mu=mu,
                                gaussian.cov_mat=Sigma,
                                adaptive = TRUE,
                                paired_shap_sampling  = TRUE,
                                adaptive_arguments = list(allow_feature_reduction = FALSE))

no_coal_standard <- expl_standard$internal$objects$X[,.N]-2


# Full computation with all 2^12 coalitions for all observations

expl_full <- shapr::explain(model = model,
                            x_explain= x_explain,
                            x_train = x_train,
                            approach = approach,
                            n_MC_samples = n_MC_samples,
                            prediction_zero = p0,
                            gaussian.mu=mu,
                            gaussian.cov_mat=Sigma,
                            adaptive = FALSE,
                            adaptive_arguments = list(allow_feature_reduction = FALSE))

expl_full$internal$objects$X[,.N] # The number of coalitions (4096)

# Rerun the regular method with the same number of coaltions used by the reduction method for each observation

expl_no_reduction_list <- list()
for(i in seq_len(nrow(x_explain))){
  expl_no_reduction_list[[i]] <- shapr::explain(model = model,
                                                x_explain= x_explain[i,],
                                                x_train = x_train,
                                                approach = approach,
                                                n_MC_samples = n_MC_samples,
                                                prediction_zero = p0,
                                                gaussian.mu=mu,
                                                gaussian.cov_mat=Sigma,
                                                adaptive = FALSE,
                                                max_n_coalitions = tot_used_coal[i],
                                                paired_shap_sampling  = paired_shap_sampling,
                                                adaptive_arguments = list(allow_feature_reduction = FALSE))
  print(i)

}

shap_vals_no_reduction_list <- list()
for (i in seq_len(nrow(x_explain))){
  shap_vals_no_reduction_list[[i]] <- expl_no_reduction_list[[i]]$shapley_values[1,]
}

no_red_shap_vals <- rbindlist(shap_vals_no_reduction_list)


# MAE per observation
MAE_obs_reduction <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-red_shap_vals[,-c(1,2,3)]))

MAE_obs_no_reduction <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-no_red_shap_vals[,-c(1,2)]))

# MAE per feature
MAE_feat_reduction <- colMeans(abs(expl_full$shapley_values[,-c(1,2)]-red_shap_vals[,-c(1,2,3)]))

MAE_feat_no_reduction <- colMeans(abs(expl_full$shapley_values[,-c(1,2)]-no_red_shap_vals[,-c(1,2)]))




