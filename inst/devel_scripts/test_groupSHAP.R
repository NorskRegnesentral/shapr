library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm","dis",
           "indus") # ,"nox","tax"
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Just looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
group1 <- list(c(1,2),
               c(3,4)) # c(4,5),c(6)
group1_names = lapply(group1, function(x){x_var[x]})

# Prepare the data for explanation
explainer1 <- shapr(x_train, model, group = group1_names) # group = group1_names

p0 <- mean(y_train)

explanation1 <- explain(x_test, explainer1, approach = "gaussian", prediction_zero = p0)

plot(explanation1)

# TODO in the end
# 6. Fix plot.shapr Currently, does not work properly. (DONE)
# 7. Test the approach on the dnb data, and see what kind of results you get for the two cases we
#    use in the paper. (DONE)
# 8. Try out group shapley as a way to explain categorical data by grouping one-hot-encoded variables,
#    and using the empirical approach to estimate the necassary conditional explectations. (DONE.)
# 9. Create/update tests whereever the behavior is different for groups. (DONE.)
# 10. Generally need to check the order of x_train and x_test. Should reorder everything to feature_labels
# and then before returning results in the end, bring it back to original of x_test.



## May 26th
## Categorical data

# 1. Simulate continuous data
# 2. Converte to categorical
# 3. One-hot encode the categorical data
# 4.1 Fit the regular empirical method to the one-hot encoded data
# 4.2 Group one-hot encoded categorical data and fit the grouped empirical to the origianl variables
# 4.3 Use ctree on the categorical data

library(MASS)
library(data.table)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/3-calculate_true_shapley_withdatatable.R")

# parameters
dim <- 3
mu <- rep(0, dim)
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
no_categories <- length(cutoff) - 1
set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
Sample_test <- TRUE
No_train_obs <- 1000
No_test_sample <- 100
N_sample_gaussian <- 1000

noise <- TRUE
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
fit_mod <- "regression"
methods <- "empirical"
seed <- 1
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)[1]
Sigma_diag <- 1
x_test_dt <-NULL


all_methods <- list()
for(cc in corr){

  print(paste0("correlation: ", cc), quote = FALSE, right = FALSE)
  Sigma <- matrix(rep(cc, dim^2), nrow = dim, ncol = dim)
  for(i in 1:dim){
    Sigma[i, i] <- Sigma_diag
  }

  ## 1. simulate training data
  set.seed(seed)
  x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
  dt <- NULL
  if(is.null(cutoff)){
    for(i in 1:no_categories){
      dt <- cbind(dt, cut(x[, i], quantile(x[, i], probs = (1 / no_categories * seq(0, no_categories, by = 1))), labels = 1:no_categories, include.lowest = TRUE)) # without include.lowest, you get NA at the boundaries
      cutoff <- c(cutoff, quantile(x[, i], probs = (1 / no_categories * seq(0, no_categories, by = 1))))
    }
    cutoff <- matrix(cutoff, nrow = dim, ncol = no_categories, byrow = TRUE)
  } else{
    for(i in 1:dim){
      dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
    }
  }


  ## Get every combination of the levels and dimensions unless x_test_dt is passed to parameters_list
  if(is.null(x_test_dt)){
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
  }

  No_test_obs <- nrow(x_test_dt)
  dt <- rbind(dt, x_test_dt)

  dt <- data.table(dt)
  setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
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
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[-(1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response)]

  x_train_numeric <- dt_numeric[(1:No_train_obs), ..feat_names]
  x_test_numeric <- dt_numeric[-(1:No_train_obs), ..feat_names]

  # For computing the true Shapley values (with correlation 0)
  x_test_onehot_full <- dt[-(1:No_train_obs), ..full_onehot_names]

  x_test_onehot_reduced <- dt[-(1:No_train_obs), ..reduced_onehot_names]
  x_train_onehot_reduced <- dt[(1:No_train_obs), ..reduced_onehot_names]

  ##
  explainer <- shapr(x_train, model)

  if(any(grepl("empirical", methods)) | any(grepl("gaussian", methods)) | any(grepl("ctree_onehot", methods))){
    explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
  }

  ## Create custom function of model_type for lm
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
  # End custom function

  ## 6. calculate the true shapley values
  print("Started calculating true Shapley values.", quote = FALSE, right = FALSE)

  set.seed(10)
  joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod)
  marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
  cond_list <- cond_prob(marg_list, joint_prob_dt_list[[1]], explainer)
  cond_expec_mat <- cond_expec_new(cond_list, explainer, x_test, prediction_zero = joint_prob_dt_list[[2]], joint_prob_dt = joint_prob_dt_list[[1]])
  rm(cond_list) # to save memory
  gc()
  true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)

  print(head(true_shapley))
  #           none   feat_1_    feat_2_     feat_3_
  # 1: -0.05576529 0.1189073  0.3437063  0.18465409
  # 2: -0.05576529 0.1189073  0.3437063  0.09758738
  # 3: -0.05576529 0.1189073  0.3437063 -0.79164041
  # 4: -0.05576529 0.1189073 -0.7561449  0.18465409
  # 5: -0.05576529 0.1189073 -0.7561449  0.09758738
  # 6: -0.05576529 0.1189073 -0.7561449 -0.79164041


  print("Finished calculating true Shapley values.", quote = FALSE, right = FALSE)

  ## 8. calculate approximate shapley value with different methods
  p <- mean(y_train$response) # since y_train is no longer a matrix

  print("Started estimating Shapley values with various methods.", quote = FALSE, right = FALSE)
  explanation_list <- list()

  ## 1. Regular empirical on one-hot encoded variables
  m <- 'empirical_regular'
  tm0 <- proc.time()
  explanation_list[[m]] <- explain(
    x_test_onehot_reduced,
    approach = "empirical",
    explainer = explainer_onehot,
    prediction_zero = p,
    sample = FALSE,
    w_threshold = 1,
    n_samples = 1000)
  tm1 <- proc.time()

  beta_matcher <- as.numeric(getstr(reduced_onehot_names))
  no_features <- max(beta_matcher)
  phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

  for (i in 1:no_features){
    phi_sum_mat[, i] <- rowSums(subset(explanation_list[[m]]$dt, select = which(beta_matcher == i) + 1))
  }
  colnames(phi_sum_mat) <- feat_names
  explanation_list[[m]]$dt_sum <- cbind(explanation_list[[m]]$dt[, 1], phi_sum_mat)
  print(head(explanation_list[[m]]$dt_sum))
  #           none    feat_1_    feat_2_     feat_3_
  # 1: 0.02618351 0.05110211  0.3219652  0.19225162
  # 2: 0.02618353 0.06494674  0.3280420  0.08526356
  # 3: 0.02618351 0.10099959  0.3108747 -0.82284984
  # 4: 0.02618350 0.08536755 -0.8078294  0.18792953
  # 5: 0.02618352 0.09264480 -0.8014691  0.08722524
  # 6: 0.02618350 0.09041001 -0.7952708 -0.80596598

  ## 2. Empirical but with groups
  m <- 'empirical_grouped'
  group1 <- list(c(1,2),
                 c(3,4),
                 c(5,6))
  group1_names = lapply(group1, function(x){names(x_test_onehot_reduced)[x]})

  explainer_grouped <- shapr(x_train_onehot_reduced, model_onehot, group = group1_names)

  tm0 <- proc.time()
  explanation_list[[m]] <- explain(
    x_test_onehot_reduced,
    approach = "empirical",
    explainer = explainer_grouped,
    prediction_zero = p,
    sample = FALSE,
    w_threshold = 1,
    n_samples = 1000)
  tm1 <- proc.time()
  explanation_list[[m]]$time <- tm1 - tm0

  return_list <- list()
  return_list[['true_shapley']] <- true_shapley
  return_list[['true_linear']] <- true_linear
  return_list[['joint_prob_true']] <- joint_prob_dt_list[[1]]
  return_list[['x_train-y_train']] <- cbind(x_train, y_train)
  return_list[['methods']] <- explanation_list
  return_list[['correlation']] <- cc
  #
  true <- return_list$true_shapley
  emp_regular <- return_list$methods$empirical_regular$dt_sum
  emp_grouped <- return_list$methods$empirical_grouped$dt
  weight_vec <- return_list$joint_prob_true[[dim + 1]]
  method_names <- c("emp_regular", "emp_grouped")

  results <- data.table(MAE = MAE(true, emp_regular, weights = weight_vec),
                        name = 'emp_regular')

  results <- rbind(results, data.table(MAE = MAE(true, emp_grouped, weights = weight_vec),
                                       name = 'emp_grouped'))
  results[, correlation := cc]
  results[, dim := dim]
  results[, no_categories := no_categories]

  return_list[['results']] <- results
  #
  y = which(cc == corr)
  all_methods[[y]] <- return_list

}

X <- NULL
for(i in 1:length(all_methods)){
  X <- rbind(X, all_methods[[i]]$results)
}

Xx <- NULL
for(i in 1:length(all_methods)){
  Xx <- c(Xx,
          all_methods[[i]]$methods$empirical_regular$time[3],
          all_methods[[i]]$methods$empirical_grouped$time[3])
}

X[, time := Xx]
X = rbind(X, data.table(MAE = 0.0274, name = 'ctree', correlation = 0, dim = 3, no_categories = 3))
X = rbind(X, data.table(MAE = 0.0191, name = 'ctree', correlation = 0.1, dim = 3, no_categories = 3))
X = rbind(X, data.table(MAE = 0.0302, name = 'ctree', correlation = 0.3, dim = 3, no_categories = 3))
X = rbind(X, data.table(MAE = 0.0310, name = 'ctree', correlation = 0.5, dim = 3, no_categories = 3))
X = rbind(X, data.table(MAE = 0.0244, name = 'ctree', correlation = 0.8, dim = 3, no_categories = 3))
X = rbind(X, data.table(MAE = 0.0259, name = 'ctree', correlation = 0.9, dim = 3, no_categories = 3))
