library(data.table)
library(MASS)
library(Matrix)
# library(shapr)
devtools::load_all()
library(future)
library(xgboost)
library(ggplot2)
library(reshape2)

m <- 12
n_train <- 5000
n_explain <- 100
rho_1 <- 0.5
rho_2 <- 0.5
rho_3 <- 0.5
rho_4 <- 0
Sigma_1 <- matrix(rho_1, m/4, m/4) + diag(m/4) * (1 - rho_1)
Sigma_2 <- matrix(rho_2, m/4, m/4) + diag(m/4) * (1 - rho_2)
Sigma_3 <- matrix(rho_3, m/4, m/4) + diag(m/4) * (1 - rho_3)
Sigma_4 <- matrix(rho_4, m/4, m/4) + diag(m/4) * (1 - rho_4)

Sigma <- as.matrix(bdiag(Sigma_1, Sigma_2, Sigma_3, Sigma_4))
mu <- rep(0,m)

set.seed(123)


x_train <- as.data.table(MASS::mvrnorm(n_train,mu,Sigma))
x_explain <- as.data.table(MASS::mvrnorm(n_explain,mu,Sigma))

names(x_train) <- paste0("VV",1:m)
names(x_explain) <- paste0("VV",1:m)


g <- function(a,b){
  a*b+a*b^2+a^2*b
}

if (m >= 8){
  beta <- c(0.2, -0.8, 1.0, 0.5, -0.8, rep(0, m - 5))
} else if (m == 4){
  beta <- c(0.2, -0.8, 1.0, 0.5)
}
gamma <- c(0.8,-1)
alpha <- 1
y_train <- alpha +
  as.vector(as.matrix(cos(x_train))%*%beta) +
  unlist(gamma[1]*g(x_train[,1],x_train[,2])) +
  unlist(gamma[1]*g(x_train[,3],x_train[,4])) +
  rnorm(n_train, 0, 1)
y_explain <- alpha +
  as.vector(as.matrix(cos(x_explain))%*%beta) +
  unlist(gamma[1]*g(x_explain[,1],x_explain[,2])) +
  unlist(gamma[1]*g(x_explain[,3],x_explain[,4])) +
  rnorm(n_train, 0, 1)

xy_train <- cbind(y_train, x_train)

set.seed(123)
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 50,
  verbose = FALSE
)

pred_train <- predict(model, as.matrix(x_train))

this_order <- order(unlist(x_train[,1]))

plot(unlist(x_train[this_order,1]),pred_train[this_order],type="l")

p0 <- mean(y_train)

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

predict_model_xgb <- function(object,newdata){
  xgboost:::predict.xgb.Booster(object,as.matrix(newdata))
}


# These are the parameters for for interative_kshap_func
n_samples <- 1000
approach = "gaussian"
kk = 1
testObs_computed <- kk
shapsum_other_features <- 0

# Reduce if < 11% prob of shapval > 0.2
shapley_threshold_prob <- 0.2
shapley_threshold_val <- 0.1
cutoff_feats <- paste0("VV",1:12)
full_pred = predict(model,as.matrix(x_explain))[testObs_computed]

# predict_model = predict_model_xgb
initial_n_combinations = min(20,2^length(cutoff_feats)-2)
# n_combinations_per_iter = 10
# max_n_combinations = NULL
# n_boot_ests = 50
# unique_sampling = TRUE
# paired_sampling = FALSE
shapley_reweighting_strategy = "on_N"
# full_pred = predict(model,as.matrix(x_explain))[testObs_computed]
# gaussian.mu = mu
# gaussian.cov_mat = Sigma
# ctree.mincriterion = 0.95
# ctree.minsplit = 20
# ctree.minbucket = 8
# ctree.sample = TRUE
# all_trees = NULL


run <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = testObs_computed,
                            cutoff_feats = cutoff_feats,
                            initial_n_combinations = initial_n_combinations,
                            full_pred = full_pred,
                            shapsum_other_features = shapsum_other_features,
                            p0 = p0,
                            predict_model = predict_model_xgb,
                            shapley_threshold_val = shapley_threshold_val,
                            shapley_threshold_prob = shapley_threshold_prob,
                            approach = approach,
                            n_samples = n_samples,
                            gaussian.mu = mu,
                            gaussian.cov_mat = Sigma,
                            shapley_reweighting_strategy = shapley_reweighting_strategy,
                            paired_sampling = TRUE)

unpaired <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = testObs_computed,
                            cutoff_feats = cutoff_feats,
                            initial_n_combinations = initial_n_combinations,
                            full_pred = full_pred,
                            shapsum_other_features = shapsum_other_features,
                            p0 = p0,
                            predict_model = predict_model_xgb,
                            shapley_threshold_val = shapley_threshold_val,
                            shapley_threshold_prob = shapley_threshold_prob,
                            approach = approach,
                            n_samples = n_samples,
                            gaussian.mu = mu,
                            gaussian.cov_mat = Sigma,
                            shapley_reweighting_strategy = shapley_reweighting_strategy,
                            paired_sampling = FALSE)
