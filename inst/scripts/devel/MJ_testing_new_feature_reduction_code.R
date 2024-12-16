library(data.table)
library(MASS)
library(Matrix)
library(shapr)
library(future)
library(xgboost)

nTrain  <- 10000
nTest   <- 100
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
  y <- 1+ 5*X[,1]+4*X[,2]+3*X[,3]+2*X[,4]+1*X[,5]
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



# These are the parameters for for interative_kshap_func
n_MC_samples <- 1000 #10000 May be increased to reduce variablity
approach = "gaussian"

ret_list <- list()

for(i in 80:100){

  expl_red <- shapr::explain(model = model,
                             x_explain= x_explain[i,],
                             x_train = x_train,
                             approach = approach,
                             n_MC_samples = n_MC_samples, # Maybe
                             prediction_zero = p0,
                             gaussian.mu=mu,
                             gaussian.cov_mat=Sigma,
                             adaptive = TRUE,
                             print_iter_info = FALSE,
                             adaptive_arguments = list(allow_feature_reduction = TRUE,
                                                       fixed_n_coalitions_per_iter = 10,
                                                       max_iter = 100,
                                                       initial_n_coalitions = 50,
                                                       shapley_threshold_val = 0.3,
                                                       shapley_threshold_prob = 0.2))


  n_iter <- length(expl_red$internal$iter_list)
  total_used_coal <- sapply(expl_red$internal$iter_list, function(sublist) sublist$total_n_coalitions)
  it_prob_of_val_above_threshold_val <- rbindlist(lapply(expl_red$internal$iter_list, function(sublist) sublist$prob_of_red)[-n_iter],fill=TRUE)

  ll <- list(total_used_coal = total_used_coal,
             it_shap_res = expl_red$internal$iter_results$dt_iter_shapley_est,
             it_shap_sd = expl_red$internal$iter_results$dt_iter_shapley_sd,
             it_prob_of_val_above_threshold_val = it_prob_of_val_above_threshold_val,
             dropped_features = expl_red$internal$iter_list[[n_iter]]$shap_reduction$dropped_features
  )
  ret_list[[i]] <- ll
  print(i)
}

expl_standard <- shapr::explain(model = model,
                                x_explain= x_explain,
                                x_train = x_train,
                                approach = approach,
                                n_MC_samples = n_MC_samples, # Maybe
                                prediction_zero = p0,
                                gaussian.mu=mu,
                                gaussian.cov_mat=Sigma,
                                adaptive = TRUE,
                                adaptive_arguments = list(allow_feature_reduction = FALSE))

no_coal_standard <- expl_standard$internal$objects$X[,.N]-2

future::plan("multisession", workers = 8) # Increase the number of workers for increased performance with many features

# Enable progress updates of the v(S)-computations
# Requires the progressr package
progressr::handlers(global = TRUE)
progressr::handlers("cli") # Using the cli package as backend (recommended for the estimates of the remaining time)

expl_full <- shapr::explain(model = model,
                                x_explain= x_explain,
                                x_train = x_train,
                                approach = approach,
                                n_MC_samples = n_MC_samples, # Maybe
                                prediction_zero = p0,
                                gaussian.mu=mu,
                                gaussian.cov_mat=Sigma,
                                adaptive = FALSE,
                                adaptive_arguments = list(allow_feature_reduction = FALSE))

save.image("MJ_testing_new_feature_reduction_code.RData")


MAE_std <- colMeans(abs(expl_full$shapley_values[,-1]-expl_standard$shapley_values[,-1]))

length(ret_list)

red_shap_vals <- rbindlist(lapply(ret_list, function(x) x$it_shap_res[.N]))

MAE_red <- colMeans(abs(expl_full$shapley_values[,-1]-red_shap_vals[,-c(1,2)]))

tot_used_coal <- sapply(ret_list, function(x) x$total_used_coal[length(x$total_used_coal)])


plot(MAE_std)
lines(MAE_red,col=2)

meanMAE_obs_std <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-expl_standard$shapley_values[,-c(1,2)]))
meanMAE_obs_red<- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-red_shap_vals[,-c(1,2,3)]))

par(mfrow=c(2,2))
plot(meanMAE_obs_std,meanMAE_obs_red,xlim=c(0,0.4),ylim=c(0,0.4))
abline(a=0,b=1,col=2)
plot(meanMAE_obs_std[tot_used_coal<no_coal_standard],meanMAE_obs_red[tot_used_coal<no_coal_standard],col="green",xlim=c(0,0.4),ylim=c(0,0.4))
points(meanMAE_obs_std[tot_used_coal>no_coal_standard],meanMAE_obs_red[tot_used_coal>no_coal_standard],col="red")
abline(a=0,b=1,col=2)


expl_standard_median_redno <- shapr::explain(model = model,
                                             x_explain= x_explain,
                                             x_train = x_train,
                                             approach = approach,
                                             n_MC_samples = n_MC_samples, # Maybe
                                             prediction_zero = p0,
                                             gaussian.mu=mu,
                                             gaussian.cov_mat=Sigma,
                                             adaptive = FALSE,
                                             max_n_coalitions  = round(median(tot_used_coal)),
                                             adaptive_arguments = list(allow_feature_reduction = FALSE))

meanMAE_obs_std_redno <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-expl_standard_median_redno$shapley_values[,-c(1,2)]))
no_coal_standard_redno <- expl_standard_median_redno$internal$objects$X[,.N]-2

plot(meanMAE_obs_std_redno,meanMAE_obs_red,xlim=c(0,0.4),ylim=c(0,0.4))
abline(a=0,b=1,col=2)
plot(meanMAE_obs_std_redno[tot_used_coal<no_coal_standard_redno],meanMAE_obs_red[tot_used_coal<no_coal_standard_redno],col="green",xlim=c(0,0.4),ylim=c(0,0.4))
points(meanMAE_obs_std_redno[tot_used_coal>no_coal_standard_redno],meanMAE_obs_red[tot_used_coal>no_coal_standard_redno],col="red")
abline(a=0,b=1,col=2)




expl_standard_redno_list <- list()
for(i in seq_len(nrow(x_explain))){
  expl_standard_redno_list[[i]] <- shapr::explain(model = model,
                                               x_explain= x_explain[i,],
                                               x_train = x_train,
                                               approach = approach,
                                               n_MC_samples = n_MC_samples, # Maybe
                                               prediction_zero = p0,
                                               gaussian.mu=mu,
                                               gaussian.cov_mat=Sigma,
                                               adaptive = FALSE,
                                               max_n_coalitions = tot_used_coal[i],
                                               adaptive_arguments = list(allow_feature_reduction = FALSE))
  print(i)

}

shap_vals_redno_exact_list <- list()
for (i in 1:100){
  shap_vals_redno_exact_list[[i]] <- expl_standard_redno_list[[i]]$shapley_values[i,]
}

shap_vals_redno_exact <- rbindlist(shap_vals_redno_exact_list)

meanMAE_obs_std_redno_exact <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-shap_vals_redno_exact[,-c(1,2)]))

plot(meanMAE_obs_std_redno_exact,meanMAE_obs_red,xlim=c(0,0.4),ylim=c(0,0.4))
abline(a=0,b=1,col=2)

MAE_std_redno_exact <- colMeans(abs(expl_full$shapley_values[,-c(1)]-shap_vals_redno_exact[,-c(1)]))

plot(MAE_std_redno_exact)
points(MAE_red,col=2)

#save.image("MJ_testing_new_feature_reduction_code.RData")


