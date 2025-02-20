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
#################################################################################################
###################################################################################################
nTrain  <- 10000
nTest   <- 10
simSeed <- 100

nVar       <- 12
corMat <- diag(nVar)
corMat[1:3,1:3] <- 0.5
corMat[4:6,4:6] <- 0.5
corMat[7:9,7:9] <- 0.5
diag(corMat) <- 1
mu <- rep(0,nVar)

predFuncT <- function(X)
{
  n <- dim(X)[1]
  eps <- rnorm(n,0,1)
  y <- 1+ 5*X[,1]+4*X[,2]+3*X[,3]+2*X[,4]+1*X[,5]+5*X[,11]*X[,12]
  y <- y + eps
}

###################################################################################################
set.seed(simSeed)
library(mvtnorm)
X       <- rmvnorm(n=nTrain,mean=mu,sigma=corMat)
Xtest   <- rmvnorm(n=nTest,mean=mu,sigma=corMat)

y <- predFuncT(X)
trainData <- as.data.frame(cbind(y,X))

ytest <- predFuncT(Xtest)
testData <- as.data.frame(cbind(ytest,Xtest))

model <- lm(y~V2+V3+V4+V5+V6+V12*V13-V12-V13, data = trainData)

model <- lm(y~.+V12*V13, data = trainData)

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
shapley_threshold_val = 0.3 # The z in the formula for when the remove feature j: Pr(|\phi_j| > z) < y
shapley_threshold_prob = 0.2 # The z in the formula for when the remove feature j: Pr(|\phi_j| > z) < y
adaptive = TRUE # Whether to compute Shapley values iteratively or not. Must be used when allow_feature_reduction = TRUE
n_MC_samples <- 1000 # Number of Monte Carlo samples used in the numerical integration of the Shapley values
approach = "gaussian" # "gaussian", "ctree", "vaeac", "independence" or similar. See ?shapr::explain for the full set of options.
gaussian.mu=mu # The mean of the Gaussian distribution used in the Gaussian approach. Set to NULL to estimate it from x_explain
gaussian.cov_mat=Sigma # The covariance matrix of the Gaussian distribution used in the Gaussian approach. Set to NULL to estimate it from x_explain

ret_list <- list()

start_time <- Sys.time()
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
                             print_iter_info = FALSE,
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
end_time <- Sys.time()
print(end_time - start_time)


# The final Shapley values for all observations with the reduction method
red_shap_vals <- rbindlist(lapply(ret_list, function(x) x$it_shap_res[.N]))

# The fianl number of coalitions used for each observations
tot_used_coal <- sapply(ret_list, function(x) x$total_used_coal[length(x$total_used_coal)])
hist(tot_used_coal)

# Plain iterative computation for all observations (just for illustration, not used for anything below)
#Kjoerer alle observasjoner samtidig
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

future::multisession()
progressr::handlers(global = TRUE)
progressr::handlers("cli")

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

expl_full$shapley_values

# obs 4 has V2 with small shapley value. Will check if v(1,S) for different S is similar to V(S) for different S

dt_vS <- expl_full$internal$output$dt_vS
X <- expl_full$internal$objects$X

X[,coalitions_str := sapply(coalitions, paste0, collapse = "_")]

coal_with_1 <- X[coalitions_str %in% c("1","1_2","1_2_3","1_2_3_4"),id_coalition]
coal_without_1 <- X[coalitions_str %in% c("","2","2_3","2_3_4"),id_coalition]

dt_vS[id_coalition %in% coal_with_1,p_hat1_4]
#[1] -0.2960277 -1.8295669 -3.0791259 -3.3204031
dt_vS[id_coalition %in% coal_without_1,p_hat1_4]
#[1]  0.9797508 -2.4882002 -3.6035339 -4.2394226


expl_red$internal$iter_list[[1]]$dt_vS
expl_red$internal$iter_list[[1]]$dt_vS_org




expl_full$internal$objects$X[,.N] # The number of coalitions (4096)

# Rerun the regular method with the same number of coaltions used by the reduction method for each observation

start_time <- Sys.time()
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
end_time <- Sys.time()
print(start_time-end_time)

shap_vals_no_reduction_list <- list()
for (i in seq_len(nrow(x_explain))){
  shap_vals_no_reduction_list[[i]] <- expl_no_reduction_list[[i]]$shapley_values[1,]
}

no_red_shap_vals <- rbindlist(shap_vals_no_reduction_list)

#### LA STANDARDMETODEN BESTEMME ANTALL KOALISASJONER SELV ##############################################################

start_time <- Sys.time()
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
                                                #adaptive = FALSE,
                                                adaptive=TRUE,
                                                #max_n_coalitions = tot_used_coal[i],
                                                paired_shap_sampling  = paired_shap_sampling,
                                                adaptive_arguments = list(allow_feature_reduction = FALSE))
  print(i)

}
end_time <- Sys.time()
print(end_time-start_time)

shap_vals_no_reduction_list <- list()
for (i in seq_len(nrow(x_explain))){
  shap_vals_no_reduction_list[[i]] <- expl_no_reduction_list[[i]]$shapley_values[1,]
}

adaptive_shap_vals <- rbindlist(shap_vals_no_reduction_list)

############################################################################################################################
######################################################################################################

#kernelShapTrue <- matrix(scan("/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Kjersti/IntegreatRetreatNov24/LinearModel1/kernelShapTrue.txt", sep=";"),ncol=13,byrow=T)

library(condMVNorm)
source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")
kernelShapTrue=matrix(0,ncol=13,nrow=100)
tmp       <- makeXmatrix(m=12, eksakt=TRUE,nRows=100)
weightMat <- tmp$weightMat
xMat      <- tmp$xMat
xMat0     <- xMat

data <- as.data.table(trainData[,-1])
phi_0    <- mean(predict(model, data))
S <- xMat0[,-1]
nRows <- dim(xMat0)[1]
vTeo <- array(0,nRows)
vTeo[1] <- phi_0
for(j in 1:nTest)
{
  print(j)
  x0 <- as.numeric(testData[j,-1])
  instance <- as.data.table(testData[j,-1])
  v_all    <- predict(model, instance)
  for(i in 2:(nRows-1))
  {
    given.ind <- which(S[i,]==1)
    dep.ind   <- which(S[i,]==0)

    condVals  <- x0[given.ind]
    tmp <- condMVN(X.given = condVals, mean = muNorm,   sigma = covMat, dependent.ind = dep.ind, given.ind = given.ind)
    vTeo[i] <- sum(coef[dep.ind]*tmp$condMean)+sum(coef[given.ind]*condVals)
  }
  vTeo[nRows] <- v_all
  kernelShapTeo <- weightMat%*%vTeo
  kernelShapTrue[j,] <- kernelShapTeo
}


####################################################################################################################################################3

kernelShapTrue <- expl_full$shapley_values[,-1]
# MAE per observation
#MAE_obs_reduction <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-red_shap_vals[,-c(1,2,3)]))
MAE_obs_reduction <-rowMeans(abs(kernelShapTrue-red_shap_vals[,-c(1,2)]))

#MAE_obs_no_reduction <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-no_red_shap_vals[,-c(1,2)]))
MAE_obs_no_reduction <-rowMeans(abs(kernelShapTrue-no_red_shap_vals[,-c(1)]))

#MAE adaptive
MAE_obs_adaptive <- rowMeans(abs(kernelShapTrue-adaptive_shap_vals[,-c(1)]))

plot.ts(cbind(MAE_obs_no_reduction,MAE_obs_reduction),plot.type="single")
lines(MAE_obs_no_reduction,col=3)

plot.ts(cbind(MAE_obs_adaptive,MAE_obs_reduction),plot.type="single")
lines(MAE_obs_adaptive,col=3)

# MAE per feature
#MAE_feat_reduction <- colMeans(abs(expl_full$shapley_values[,-c(1,2)]-red_shap_vals[,-c(1,2,3)]))
MAE_feat_reduction <- colMeans(abs(kernelShapTrue-red_shap_vals[,-c(1,2)]))

#MAE_feat_no_reduction <- colMeans(abs(expl_full$shapley_values[,-c(1,2)]-no_red_shap_vals[,-c(1,2)]))
MAE_feat_no_reduction <- colMeans(abs(kernelShapTrue-no_red_shap_vals[,-c(1)]))

#MAE_feat_no_reduction <- colMeans(abs(expl_full$shapley_values[,-c(1,2)]-no_red_shap_vals[,-c(1,2)]))
MAE_feat_adaptive <- colMeans(abs(kernelShapTrue-adaptive_shap_vals[,-c(1)]))


plot.ts(cbind(MAE_feat_no_reduction[-1],MAE_feat_reduction[-1]),plot.type="single")
lines(MAE_feat_no_reduction[-1],col=3)

plot.ts(cbind(MAE_feat_adaptive[-1],MAE_feat_reduction[-1]),plot.type="single")
lines(MAE_feat_adaptive[-1],col=3)


testObs <- 26
plot.ts(cbind(kernelShapTrue[testObs,-1],as.numeric(red_shap_vals[testObs,-c(1,2,3)]), as.numeric(no_red_shap_vals[testObs,-c(1,2)])),plot.type="single")
lines(as.numeric(red_shap_vals[testObs,-c(1,2,3)]),col=2)
lines(as.numeric(no_red_shap_vals[testObs,-c(1,2)]),col=3)



