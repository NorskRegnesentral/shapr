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

expl_standard <- shapr::explain(model = model,
                                x_explain= x_explain,
                                x_train = x_train,
                                approach = approach,
                                n_MC_samples = n_MC_samples, # Maybe
                                prediction_zero = p0,
                                gaussian.mu=mu,
                                gaussian.cov_mat=Sigma,
                                adaptive = TRUE,
                                adaptive_arguments = list(allow_feature_reduction = FALSE,
                                                          shapley_threshold_val = 0.3,
                                                          shapley_threshold_prob = 0.3))

no_coal_standard <- expl_standard$internal$objects$X[,.N]-2

ret_list <- list()

for(i in seq_len(nrow(x_explain))){

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
                                                       shapley_threshold_val = 0.3,
                                                       shapley_threshold_prob = 0.3))


  n_iter <- length(expl_red$internal$iter_list)
  used_coal <- length(unique(expl_red$internal$iter_list[[1]]$coal_samples_org))
  for(j in seq_len(n_iter-2)){
    this <- length(unique(expl_red$internal$iter_list[[j+1]]$coal_samples_org))-length(unique(expl_red$internal$iter_list[[j]]$coal_samples))
    used_coal <- c(used_coal,this)
  }
  last <- expl_red$internal$objects$X[,.N]-length(unique(expl_red$internal$iter_list[[n_iter-1]]$coal_samples))
  used_coal <- c(used_coal,last)
  total_used_coal <- cumsum(used_coal)

  ll <- list(total_used_coal = total_used_coal,
             it_shap_res = expl_red$internal$iter_results$dt_iter_shapley_est,
             dropped_features = expl_red$internal$iter_list[[n_iter]]$shap_reduction$dropped_features
  )
  ret_list[[i]] <- ll
  print(i)
}


expl_red$internal$iter_results

internal$iter_list[[1]]$n_coalitions_org
internal$iter_list[[2]]$n_coalitions_org-internal$iter_list[[1]]$n_coalitions
internal$iter_list[[3]]$n_coalitions_org-internal$iter_list[[2]]$n_coalitions
internal$iter_list[[4]]$n_coalitions_org-internal$iter_list[[3]]$n_coalitions
internal$iter_list[[5]]$n_coalitions_org-internal$iter_list[[4]]$n_coalitions
internal$iter_list[[6]]$n_coalitions_org-internal$iter_list[[5]]$n_coalitions


length(unique(internal$iter_list[[1]]$coal_samples_org))
length(unique(internal$iter_list[[2]]$coal_samples_org))-length(unique(internal$iter_list[[1]]$coal_samples))
length(unique(internal$iter_list[[3]]$coal_samples_org))-length(unique(internal$iter_list[[2]]$coal_samples))
length(unique(internal$iter_list[[4]]$coal_samples_org))-length(unique(internal$iter_list[[3]]$coal_samples))
length(unique(internal$iter_list[[5]]$coal_samples_org))-length(unique(internal$iter_list[[4]]$coal_samples))
length(unique(internal$iter_list[[6]]$coal_samples_org))-length(unique(internal$iter_list[[5]]$coal_samples))
length(unique(internal$iter_list[[7]]$coal_samples_org))-length(unique(internal$iter_list[[6]]$coal_samples))



expl_red$internal$iter_results$dt_iter_shapley_est
no_iter <- nrow(expl_red$internal$iter_results$dt_iter_shapley_est)
expl_red$internal$iter_list[[no_iter]]$shap_reduction$dropped_features


explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = approach,
  prediction_zero = p0,
  adaptive = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE
),





