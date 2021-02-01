library(MASS)
library(data.table)
library(shapr)

source("inst/devel_scripts/create_exact_joint_prob.R")

# parameters
dim <- 3
mu <- rep(0, dim)
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
no_categories <- length(cutoff) - 1
set.seed(1); beta <- rnorm(dim * no_categories + 1)

# set.seed(1); beta <- rnorm((dim * no_categories + 1)*10)
# beta_list <- split(beta, ceiling(seq_along(beta) / (dim * no_categories + 1)))

Sample_test <- TRUE

No_train_obs <- 1000

No_test_sample <- 100

N_sample_gaussian <- 1000

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}

corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)

# group2_1 <- list(c('feat_1_', 'feat_3_'), c('feat_2_', 'feat_4_'))
# group2_2 <- list(c('feat_1_', 'feat_3_', 'feat_4_'), c('feat_2_'))
# group2_3 <- list(c('feat_1_', 'feat_2_'), c('feat_3_', 'feat_4_'))
# group2_4 <- list(c('feat_2_', 'feat_3_', 'feat_4_'), c('feat_1_'))

# groupings <- list(group2_1, group2_2, group2_3, group2_4)

results <- NULL

for(cc in corr) {

  print(paste0("Correlation: ", cc))

  Sigma <- matrix(rep(cc, dim^2), nrow = dim, ncol = dim)
  for(i in 1:dim){
    Sigma[i, i] <- 1
  }

  ## 1. simulate training data
  set.seed(1)
  x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)

  dt <- NULL
  for(i in 1:dim){
    dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
  }

  ## Get test data
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
  dt <- data.table(rbind(dt, x_test_dt))
  setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
  feat_names <- names(dt[, 1:dim])

  dt <- dt[, lapply(.SD, as.factor)]

  set.seed(1)
  epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
  epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
  epsilon <- c(epsilon1, epsilon2)

  dt[, epsilon := epsilon]

  ## 2. One-hot encoding of training data
  mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts,
                                                                              contrasts = FALSE))
  dt <- cbind(dt, data.table(mod_matrix))
  full_onehot_names <- colnames(mod_matrix)

  mod_matrix_not_complete <- model.matrix(~., data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts,
                                                                                         contrasts = TRUE))
  reduced_onehot_names <- colnames(mod_matrix_not_complete)
  reduced_onehot_names <- reduced_onehot_names[reduced_onehot_names != "(Intercept)"]

  ## 3. Calculate response
  dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

  ## 4. Fit model
  form <- as.formula(paste0("response ~", paste(feat_names, collapse = "+")))
  model <- lm(formula = form, data = dt[(1:No_train_obs), ])

  fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = " + ")))
  model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])

  ## 5. initalize shapr object with trained model -- this is used for calculating true shapley
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[-(1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response)]

  # For computing the true Shapley values (with correlation 0)
  x_test_onehot_reduced <- dt[-(1:No_train_obs), ..reduced_onehot_names]
  x_train_onehot_reduced <- dt[(1:No_train_obs), ..reduced_onehot_names]

  explainer <- shapr(x_train, model)
  explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)

  ##  -------------------------------------------
  joint_prob_dt <- create_exact_joint_prob(mu = mu, Sigma = Sigma, explainer = explainer, cutoff = cutoff)
  #print(joint_prob_dt[])
  #print(sum(joint_prob_dt$joint_prob))

  p <- mean(y_train$response)

  set.seed(1)
  explanation_truth <- explain(
    x_test,
    approach = "categorical",
    explainer = explainer,
    prediction_zero = p,
    joint_prob_dt = joint_prob_dt
  )
  set.seed(1)
  explanation_ctree <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    n_samples = 1000)

  ## empirical - group before
  set.seed(1)
  group_onehot_names <- group_factors(x_train)
  explainer_onehot_group <- shapr(x_train_onehot_reduced, model_onehot, group = group_onehot_names)

  explanation_empirical_group_before <- explain(
    x_test_onehot_reduced,
    approach = "empirical",
    explainer = explainer_onehot_group,
    prediction_zero = p)

  ## empirical - group after
  set.seed(1)
  explanation_empirical_group_after <- explain(
    x_test_onehot_reduced,
    approach = "empirical",
    explainer = explainer_onehot,
    prediction_zero = p)

  beta_matcher <- as.numeric(getstr(reduced_onehot_names))
  no_features <- max(beta_matcher)
  phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

  for (i in 1:no_features){
    phi_sum_mat[, i] <- rowSums(subset(explanation_empirical_group_after$dt, select = which(beta_matcher == i) + 1))
  }
  colnames(phi_sum_mat) <- feat_names
  explanation_empirical_group_after$dt_sum <- cbind(explanation_empirical_group_after$dt[, 1], phi_sum_mat)

  ctree_results <- MAE(explanation_truth$dt, explanation_ctree$dt, weights= joint_prob_dt$joint_prob)
  empirical_group_before_results <- MAE(explanation_truth$dt, explanation_empirical_group_before$dt, weights= joint_prob_dt$joint_prob)
  empirical_group_after_results <- MAE(explanation_truth$dt, explanation_empirical_group_after$dt_sum, weights = joint_prob_dt$joint_prob)

  ## gaussian - group before
  set.seed(1)

  explanation_gaussian_group_before <- explain(
    x_test_onehot_reduced,
    approach = "gaussian",
    explainer = explainer_onehot_group,
    prediction_zero = p)

  ## gaussian - group after
  set.seed(1)
  explanation_gaussian_group_after <- explain(
    x_test_onehot_reduced,
    approach = "gaussian",
    explainer = explainer_onehot,
    prediction_zero = p)

  beta_matcher <- as.numeric(getstr(reduced_onehot_names))
  no_features <- max(beta_matcher)
  phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

  for (i in 1:no_features){
    phi_sum_mat[, i] <- rowSums(subset(explanation_gaussian_group_after$dt, select = which(beta_matcher == i) + 1))
  }
  colnames(phi_sum_mat) <- feat_names
  explanation_gaussian_group_after$dt_sum <- cbind(explanation_gaussian_group_after$dt[, 1], phi_sum_mat)

  ## results
  ctree_results <- MAE(explanation_truth$dt, explanation_ctree$dt, weights= joint_prob_dt$joint_prob)
  empirical_group_before_results <- MAE(explanation_truth$dt, explanation_empirical_group_before$dt, weights= joint_prob_dt$joint_prob)
  empirical_group_after_results <- MAE(explanation_truth$dt, explanation_empirical_group_after$dt_sum, weights = joint_prob_dt$joint_prob)
  gaussian_group_before_results <- MAE(explanation_truth$dt, explanation_gaussian_group_before$dt, weights= joint_prob_dt$joint_prob)
  gaussian_group_after_results <- MAE(explanation_truth$dt, explanation_gaussian_group_after$dt_sum, weights = joint_prob_dt$joint_prob)

  results <- rbind(results, c(ctree_results, empirical_group_before_results, empirical_group_after_results, gaussian_group_before_results, gaussian_group_after_results))

}

results_dt = data.table(results)

names(results_dt) <- c("ctree", "empirical_before", "empirical_after", "Gaussian_before", "Gaussian_after")
results_dt <- cbind(corr, results_dt)

# corr      ctree empirical_before empirical_after Gaussian_before Gaussian_after
# 1:  0.0 0.01662900       0.02310312      0.02248736      0.02836571     0.02653331
# 2:  0.1 0.01548952       0.02603541      0.02456474      0.02799519     0.02512334
# 3:  0.3 0.02728691       0.02965331      0.03466902      0.03485370     0.03563984
# 4:  0.5 0.03047182       0.02760880      0.03736145      0.03285291     0.03590932
# 5:  0.8 0.02353138       0.02782304      0.04380103      0.02925802     0.03613433
# 6:  0.9 0.01825402       0.01954248      0.03821073      0.02864430     0.04460825





