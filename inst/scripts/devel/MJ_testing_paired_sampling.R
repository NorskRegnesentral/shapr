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

i = 1

expl_red <- shapr::explain(model = model,
                           x_explain= x_explain[i,],
                           x_train = x_train,
                           approach = approach,
                           n_MC_samples = n_MC_samples, # Maybe
                           prediction_zero = p0,
                           paired_shap_sampling  = TRUE,
                           gaussian.mu=mu,
                           gaussian.cov_mat=Sigma,
                           adaptive = TRUE,
                           print_iter_info = TRUE,
                           adaptive_arguments = list(allow_feature_reduction = TRUE,
                                                     fixed_n_coalitions_per_iter = 10,
                                                     max_iter = 100,
                                                     initial_n_coalitions = 50,
                                                     shapley_threshold_val = 0.2,
                                                     shapley_threshold_prob = 0.2))


n_iter <- length(expl_red$internal$iter_list)
total_used_coal <- sapply(expl_red$internal$iter_list, function(sublist) sublist$total_n_coalitions)
it_prob_of_val_above_threshold_val <- rbindlist(lapply(expl_red$internal$iter_list, function(sublist) sublist$prob_of_red)[-n_iter],fill=TRUE)

ll <- list(total_used_coal = total_used_coal,
           it_shap_res = expl_red$internal$iter_results$dt_iter_shapley_est,
           it_shap_sd = expl_red$internal$iter_results$dt_iter_shapley_sd,
           it_prob_of_val_above_threshold_val = it_prob_of_val_above_threshold_val,
           dropped_features = expl_red$internal$iter_list[[n_iter]]$shap_reduction$dropped_features

