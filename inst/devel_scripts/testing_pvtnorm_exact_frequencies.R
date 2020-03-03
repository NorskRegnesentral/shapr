
#### Devel script for exact probabilitites using pvtnorm for categorical distributions


library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

test <- FALSE
tod_date <- format(Sys.Date(), "%d_%m_%y")

dim <- 4
no_categories <- 3
cutoff = c(-200, 0, 1, 200)
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

clock_seed_0 <- round(as.numeric(Sys.time()) * 1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1, 5)
print(rand_string)
folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/", folder, sep = ""))
dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations/", folder, sep = ""))

##
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}


parameters_list <- list()

if(test){
  print("Testing.")
  set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
  corr <- c(0, 0.1)
  k <- 1
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = rep(0, dim),
                                 beta = beta,
                                 N_shapley = 1e+03,
                                 noise = TRUE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = methods,
                                 name = paste0('corr', j),
                                 cutoff = cutoff,
                                 Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                                 No_test_sample = 2,
                                 No_train_obs = 100,
                                 N_sample_gaussian = c(50),
                                 seed = 1,
                                 no_categories = no_categories)
    k <- k + 1
  }

} else{
  set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
  corr <- c(0, 0.1, 0.5, 0.8, 0.9)
  k <- 1
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = rep(0, dim),
                                 beta = beta,
                                 N_shapley = 1e+07,
                                 noise = TRUE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = methods,
                                 name = paste0('corr', j),
                                 cutoff = cutoff,
                                 Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                                 No_test_sample = 20,
                                 No_train_obs = 1000,
                                 N_sample_gaussian = c(100, 1000),
                                 seed = 1,
                                 no_categories = no_categories)
    k <- k + 1
  }

}
parameters_list = parameters_list[[3]]

timeit <- list()

#
mu <- parameters_list$mu
beta <- parameters_list$beta
N_shapley <- parameters_list$N_shapley # number of var to simulate to calc true Shapley
Sample_test <- parameters_list$Sample_test
No_train_obs <- parameters_list$No_train_obs
No_test_sample <- parameters_list$No_test_sample
N_sample_gaussian <- parameters_list$N_sample_gaussian
cutoff <- parameters_list$cutoff
noise <- parameters_list$noise
response_mod <- parameters_list$response_mod
fit_mod <- parameters_list$fit_mod
methods <- parameters_list$methods
seed <- parameters_list$seed
no_categories <- parameters_list$no_categories
if(is.null(seed)) seed <- 1
dim <- length(mu)


# check correct input
if(!all(is.numeric(mu))){
  stop("mu vector must contain only numerics.")
}
if(!all(is.numeric(beta))){
  stop("beta vector must contain only numerics.")
}
if(!is.numeric(N_shapley)){
  stop("N_shapley must be a numeric.")
}
if(Sample_test){
  if(!is.numeric(No_test_sample)){
    stop("If Sample_test is TRUE, No_test_sample must be a numeric.")
  }
}
if(!is.numeric(No_train_obs)){
  stop("No_train_obs must be a numeric.")
}
if(!is.boolean(noise)){
  stop("noise must be a boolean.")
}
if(!((length(beta) - 1) %% dim == 0)){
  stop("beta variable - 1 must be divisible by length of mu parameter.")
}
if((length(cutoff) - 1) != no_categories){
  if(!is.null(cutoff)){
    stop("cutoff vector must either be length of no_categories plus 1 or be NULL.")
  }
}

## make sure Sigma is positive definite
Sigma <- matrix(rep(parameters_list$corr, dim^2), dim, dim)
for(i in 1:dim){
  Sigma[i, i] <- parameters_list$Sigma_diag
}
if(!lqmm::is.positive.definite(Sigma)) {
  print("Covariance matrix is not positive definite but will be converted.")
  Sigma <- make.positive.definite(Sigma)
  print("New Sigma matrix:")
  print(Sigma)
}

print(paste0("Dimension: ", dim), quote = FALSE, right = FALSE)
print(paste0("Number of categories: ", no_categories), quote = FALSE, right = FALSE)
print(paste0("N_shapley: ", N_shapley), quote = FALSE, right = FALSE)
print(paste0("No_train_obs: ", No_train_obs), quote = FALSE, right = FALSE)

## 1. simulate training data
tm_current <- Sys.time()
set.seed(seed)
x1 <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
x <- x1

dt <- NULL
if(is.null(cutoff)){ ## to get equal proportion in each level
  for(i in 1:no_categories){
    dt <- cbind(dt, cut(x[, i], quantile(x[, i], probs = (1 / no_categories * seq(0, no_categories, by = 1))), labels = 1:no_categories, include.lowest = TRUE)) # without include.lowest, you get NA at the boundaries
    cutoff <- c(cutoff, quantile(x[, i], probs = (1 / no_categories * seq(0, no_categories, by = 1))))
  }
  cutoff <- matrix(cutoff, nrow = dim, ncol = no_categories, byrow = TRUE)
} else{ # Annabelle checked the code chunk above
  for(i in 1:dim){
    dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
  }
}


## 1.2 Get every combination of the levels and dimensions
x_test_list <- list()
for(i in 1:dim){
  x_test_list[[i]] <- 1:no_categories
}
x_test_dt <- do.call(CJ, x_test_list)

if(Sample_test){
  if(nrow(x_test_dt) > No_test_sample){
    sampled_rows <- sample(1:nrow(x_test_dt), size = No_test_sample, replace = FALSE)
    x_test_dt <- x_test_dt[sampled_rows, ]
  }
}

No_test_obs <- nrow(x_test_dt)
dt <- rbind(dt, x_test_dt)

dt <- data.table(dt)
setnames(dt, names(dt), paste0("feat_", 1:dim,"_"))
feat_names <- names(dt[, 1:dim])

dt_numeric <- dt
dt <- dt[, lapply(.SD, as.factor)]

set.seed(seed)
if(noise == TRUE){
  epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
  epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
  epsilon <- c(epsilon1, epsilon2)

  dt_numeric[, epsilon := epsilon]
  dt[, epsilon := epsilon]
} else{
  dt_numeric[, epsilon := 0]
  dt[, epsilon := 0]
}

## 2. One-hot encoding of training data
mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts, contrasts = FALSE))

dt <- cbind(dt, data.table(mod_matrix))
full_onehot_names <- colnames(mod_matrix)
reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]
dt_numeric[, response := dt[['response']]]


## 4. Fit model
if(fit_mod == 'regression'){
  form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
  model <- lm(formula = form, data = dt[(1:No_train_obs), ])

  fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = " + ")))
  model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])
}

## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names] ## used in explainer()
x_test <- dt[-(1:No_train_obs), ..feat_names] ## used in cond_expec_mat()
y_train <- dt[(1:No_train_obs), .(response)] ## used in cond_expec_mat()

x_train_numeric <- dt_numeric[(1:No_train_obs), ..feat_names] ## used in explainer()
x_test_numeric <- dt_numeric[-(1:No_train_obs), ..feat_names] ## used in cond_expec_mat()

# For computing the true Shapley values (with correlation 0)
x_test_onehot_full <- dt[-(1:No_train_obs), ..full_onehot_names]

x_test_onehot_reduced <- dt[-(1:No_train_obs), ..reduced_onehot_names]
x_train_onehot_reduced <- dt[(1:No_train_obs), ..reduced_onehot_names]

##
explainer <- shapr(x_train, model) # print(class(model)) # "lm"

if(any(grepl("empirical", methods))){
  explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
} else if(any(grepl("gaussian", methods))){
  explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
} else if((any(grepl("ctree_onehot", methods)))){
  explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
}


## NEW
# Create custom function of model_type for lm
model_type.numeric_lm <<- function(x) {
}

features.numeric_lm <<- function(x, cnms, feature_labels = NULL) {
  if (!is.null(feature_labels)) message_features_labels()

  nms <- tail(all.vars(x$terms), -1)
  if (!all(nms %in% cnms)) error_feature_labels()
  return(nms)
}

# Create custom function of predict_model for caret
predict_model.numeric_lm <<- function(x, newdata) {
  newdata <- as.data.table(newdata)
  newdata0 <- newdata[, lapply(.SD, as.factor)]
  class(x) <- "lm"
  predict(x, newdata0)
}

class(model) <- "numeric_lm"
explainer_numeric <- shapr(x_train_numeric, model)
## END

set.seed(10)
## 6. calculate the true shapley values
tm_true_Shapley <- Sys.time();
print("Started to calculate true Shapley values.", quote = FALSE, right = FALSE)




##### CODE I WANT TO REPLACE ####

N_shapley <- 10^7

start = proc.time()
joint_prob_dt_list <- sim_true_Normal(mu, Sigma, beta, N_shapley = N_shapley, explainer, cutoff, response_mod)
end <- proc.time()
end-start
tm0 <- proc.time();
marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
tm1 <- proc.time();
timeit['Calculate_marginal_distributions'] <- list((tm1 - tm0))

tm0 <- proc.time();
cond_list <- cond_prob(marg_list, joint_prob_dt_list[[1]], explainer)
tm1 <- proc.time();
timeit['Calculate_conditional_distributions'] <- list((tm1 - tm0))

tm0 <- proc.time();
cond_expec_mat <- cond_expec_new(cond_list, explainer, x_test, prediction_zero = joint_prob_dt_list[[2]], joint_prob_dt = joint_prob_dt_list[[1]])
tm1 <- proc.time();
timeit['Calculate_expectations_distributions'] <- list((tm1 - tm0))

tm0 <- proc.time();
true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)
tm1 <- proc.time();
timeit['Calculate_true_Shapley'] <- list((tm1 - tm0))

true_shapley_old = true_shapley

start = proc.time()
joint_prob_dt_list_exact <- create_exact_joint_prob(mu,Sigma, beta, explainer, cutoff, response_mod)
end <- proc.time()

joint_prob_dt_list <- joint_prob_dt_list_exact

tm0 <- proc.time();
marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
tm1 <- proc.time();
timeit['Calculate_marginal_distributions'] <- list((tm1 - tm0))

tm0 <- proc.time();
cond_list <- cond_prob(marg_list, joint_prob_dt_list[[1]], explainer)
tm1 <- proc.time();
timeit['Calculate_conditional_distributions'] <- list((tm1 - tm0))

tm0 <- proc.time();
cond_expec_mat <- cond_expec_new(cond_list, explainer, x_test, prediction_zero = joint_prob_dt_list[[2]], joint_prob_dt = joint_prob_dt_list[[1]])
tm1 <- proc.time();
timeit['Calculate_expectations_distributions'] <- list((tm1 - tm0))

tm0 <- proc.time();
true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)
tm1 <- proc.time();
timeit['Calculate_true_Shapley'] <- list((tm1 - tm0))

true_shapley
true_shapley_old


