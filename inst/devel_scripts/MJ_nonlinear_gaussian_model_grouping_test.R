### Testing independence between groups

library(data.table)
library(shapr)


# parameters
dim <- 4
mu <- rep(0, dim)
Sample_test <- TRUE
No_train_obs <- 1000
no_beta_samp = 10
No_test_sample <- 10
N_sample_gaussian <- 1000
noise <- FALSE
beta = c(1,1,2)
response_mod <- function(data_mat,beta, epsilon){
  as.vector(beta[1] + beta[2]*sin(data_mat[,1]*3*data_mat[,2]^2) +
              beta[3]*data_mat[,3]*cos(data_mat[,3]*data_mat[,4])) +epsilon
}
fit_mod <- "regression"
seed <- 1
Sigma_diag <- 1


group2 <- list(c('feat_1_',"feat_2_"), c( 'feat_3_', 'feat_4_'))

feat_names = sort(unlist(group2))
group_num <- lapply(group2,FUN = function(x){match(x, feat_names)})
group_vec = rep(NA,dim)
for (i in 1:length(group2)){
  group_vec[group_num[[i]]] = i
}

cc = 0.4

Sigma <- matrix(rep(cc, dim^2), nrow = dim, ncol = dim)
for (i in 1:dim){
  for(j in 1:dim){
    if (group_vec[i]!=group_vec[j]){
      Sigma[i,j] = 0
    } else {
      Sigma[i,j] = cc
    }
  }
}
diag(Sigma) = Sigma_diag

l = 1
set.seed(l);  beta <- round(rnorm(3), 1)
#beta[1] = 0

## 1. simulate training data
set.seed(seed)
x <- MASS::mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)

dt <- as.data.table(x)
set.seed(seed+1)
x_test = MASS::mvrnorm(n =  No_test_sample, mu = mu, Sigma = Sigma)
x_test_dt = as.data.table(x_test)

No_test_obs <- nrow(x_test_dt)
dt <- data.table(rbind(dt, x_test_dt))
setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
feat_names <- names(dt[, 1:dim])

set.seed(seed)
if(noise == TRUE){
  epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
  epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
  epsilon <- c(epsilon1, epsilon2)

  dt[, epsilon := epsilon]
} else{
  dt[, epsilon := 0]
}

x_traintest_mat = as.matrix(dt[,..feat_names])

## 3. Calculate response
dt[, response := response_mod(data_mat = x_traintest_mat, beta = beta, epsilon = epsilon)]

model = list(beta=beta,pred_func = response_mod)
class(model) = "custom"


model_type.custom <- function(x) {
  "regression"
}

# Create custom function
predict_model.custom <- function(x, newdata) {
  unlist(x$pred_func(newdata,x$beta,0),use.names = F) # Gives a standard vector
}

predict_model(model,as.matrix(dt[,..feat_names]))


## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[-(1:No_train_obs), ..feat_names]
y_train <- dt[(1:No_train_obs), .(response)]


explainer <- shapr(x_train, model,feature_labels = names(x_train))

## Start grouping stuff
group1 <- list(c(1), # no groups at all
               c(2),
               c(3),
               c(4))
group1_names = lapply(group1, function(x){names(x_test)[x]})
explainer_group1 <- shapr(x_train, model, group = group1_names,feature_labels = names(x_train))


#group2_names = lapply(group2, function(x){names(x_test)[x]})
group2_names <- group2
explainer_group2 <- shapr(x_train, model, group = group2_names,feature_labels = names(x_train))

#### Here I need to compute the conditional expectation matrix


### Direct approaches using exact Gaussian dist

prediction_zero = predict_model(model,matrix(mu,nrow=1))


direct_kshap2 = explain(x_test,explainer_group2,
                        approach = "gaussian",
                        mu=mu,
                        cov_mat = Sigma,
                        n_samples = 10^6,
                        prediction_zero = prediction_zero)

direct_kshap0 = explain(x_test,explainer,
                        approach = "gaussian",
                        mu=mu,
                        cov_mat = Sigma,
                        n_samples = 10^6,
                        prediction_zero = prediction_zero)


##  -------------------------------------------

names <- NULL
for(i in 1:length(group2)){
  names <- c(names, paste0('group', i))
#  Kshap0_dt[, paste0('group', i) := rowSums(.SD), .SDcols = group2[[i]]]
  direct_kshap0$dt[, paste0('group', i) := rowSums(.SD), .SDcols = group2[[i]]]
}
feat_names = names(x_train)
results0 <- data.table(correlation = cc,
#                       base_MAE = mean(rowMeans(abs(Kshap0 - Kshap1)[,-1])),
#                       group_MAE = mean(rowMeans(abs(Kshap0_dt[,..names] - Kshap2[,-1]))),
#                       direct_MAE = mean(rowMeans(abs(Kshap0[,-1] - direct_kshap0$dt[,..feat_names]))),
                       direct_group_MAE = mean(rowMeans(abs(direct_kshap0$dt[,..names] - direct_kshap2$dt[,-1]))))
for(i in 1:length(group2)){
  results0[, paste0('group', i) := paste0(group2[[i]], collapse = ", ") ]
}
results0[,beta:=list(beta)]

results0
#Kshap0_dt
direct_kshap0$dt

#Kshap2
direct_kshap2$dt

