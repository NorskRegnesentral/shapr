# Read this: experiment 1 includes:
# 10 features, 2 different groupings
# correlations between features ranging from 0 to 0.9
# 1 linear model with no interactions between or within groups

library(shapr)
library(MASS)
library(data.table)

MAD <- function(pre_grouped, post_grouped, weights){
  mean(colSums((abs(pre_grouped - post_grouped)) * weights))
}

MDR <- function(ranking_pre_grouped, ranking_post_grouped, weights){
  mean(colSums((abs(ranking_pre_grouped - ranking_post_grouped)) * weights))
}

# parameters
dim <- 10
mu <- rep(0, dim)
seed = 1
set.seed(seed)
beta <- round(rnorm(dim + 1), 1)
coefs = c(-0.6, 0.2, -0.8, 1.6, 0.3, -0.8, 0.5, 0.7, 0.6, -0.3, 1.5)
fit_mod <- "regression"
methods <- "gaussian"
No_test_obs <- 100
No_train_obs <- 1000
#

results_group1 = NULL
results_group2 = NULL

corr_vec = c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
corr_vec = c(0)
# corr_vec = seq(0, 0.99, by = 0.05)
# corr_vec = c(0, 0.1)
for(corr in corr_vec){
  Sigma_diag <- 1
  Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
  for (i in 1:dim) {
    Sigma[i, i] <- Sigma_diag
  }

  ## 1. Simulate training data
  set.seed(seed)
  dt <- data.table(mvrnorm(n =  No_train_obs + No_test_obs, mu = mu, Sigma = Sigma))
  setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
  feat_names <- names(dt[, 1:dim])

  set.seed(seed)
  dt[, epsilon := rnorm(No_train_obs + No_test_obs, 0, 0.1^2)]


  ## 2. Define model
  mod_matrix <- model.matrix(~ . -1, data = dt[, 1:dim])
  # dt <- cbind(dt, data.table(mod_matrix))

  ## 3. Calculate response
  response_mod <- response_mod <- function(mod_matrix_full, beta, epsilon) {
    as.vector(mod_matrix_full %*% beta) + epsilon
  }

  dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

  dt[, mean_response := mean(response)]
  dt[, sd_response := sd(response)]
  dt[, response_stand := (response - mean_response) / sd_response]
  dt[, mean_response := NULL]
  dt[, sd_response := NULL]
  dt[, response := NULL]

  ## 4. Fit model for regression
  form <- as.formula(paste0("response_stand ~", paste(feat_names, collapse = "+")))
  model <- lm(formula = form, data = dt[(1:No_train_obs)])

  ## 5. Initalize shapr object
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[- (1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response_stand)]

  p <- mean(y_train$response_stand)

  # pre-grouping approach 1
  group1 <- list(1:5, 6:8, 9:10)
  group1 = lapply(group1, function(x){names(x_test)[x]})

  explainer_group1 <- shapr(x_train, model, group = group1)

  explanation_group1 <- explain(
    x = x_test,
    explainer = explainer_group1,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = 5000
  )

  group1_names = names(explainer_group1$group)
  rank_group_names1 = paste0(group1_names, "_rank")

  explanation_mat_pre = as.matrix(explanation_group1$dt[, ..group1_names])
  explanation_ranking_pre = t(apply(-explanation_mat_pre, FUN = rank, 1))
  colnames(explanation_ranking_pre) = rank_group_names1
  explanation_group1$dt = cbind(explanation_group1$dt, explanation_ranking_pre)

  # pre-grouping approach 2
  group2 <- list(1:2, 3:4, 5:6, 7:8, 9:10)
  group2 = lapply(group2, function(x){names(x_test)[x]})

  explainer_group2 <- shapr(x_train, model, group = group2)

  explanation_group2 <- explain(
    x = x_test,
    explainer = explainer_group2,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = 5000
  )

  group2_names = names(explainer_group2$group)
  rank_group_names2 = paste0(group2_names, "_rank")

  explanation_mat_pre2 = as.matrix(explanation_group2$dt[, ..group2_names])
  explanation_ranking_pre2 = t(apply(-explanation_mat_pre2, FUN = rank, 1))
  colnames(explanation_ranking_pre2) = rank_group_names2
  explanation_group2$dt = cbind(explanation_group2$dt, explanation_ranking_pre2)

  # post-grouping approach
  explainer <- shapr(x_train, model)
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = 5000
  )

  # compare group 1
  explanation_base1 = data.table(explanation$dt)
  explanation_base1[, group1 := rowSums(.SD), .SDcols = group1[[1]]]
  explanation_base1[, group2 := rowSums(.SD), .SDcols = group1[[2]]]
  explanation_base1[, group3 := rowSums(.SD), .SDcols = group1[[3]]]

  explanation_mat_post1 = as.matrix(explanation_base1[, ..group1_names])
  explanation_ranking_post1 = t(apply(-explanation_mat_post1, FUN = rank, 1))
  colnames(explanation_ranking_post1) = rank_group_names1
  explanation_base1 = cbind(explanation_base1, explanation_ranking_post1)

  pre_grouped = explanation_group1$dt[, ..group1_names]
  post_grouped = explanation_base1[, ..group1_names]

  pre_grouped_rank = explanation_group1$dt[, ..rank_group_names1]
  post_grouped_rank = explanation_base1[, ..rank_group_names1]

  results_group1[[length(results_group1) + 1]] = list(correlation = corr,
                                                      # pre_grouped = pre_grouped,
                                                      # post_grouped = post_grouped,
                                                      MAD = MAD(pre_grouped, post_grouped, weights = 1),
                                                      MDR = MDR(pre_grouped_rank, post_grouped_rank, weights = 1))
  # compare group 2
  explanation_base2 = data.table(explanation$dt)
  explanation_base2[, group1 := rowSums(.SD), .SDcols = group2[[1]]]
  explanation_base2[, group2 := rowSums(.SD), .SDcols = group2[[2]]]
  explanation_base2[, group3 := rowSums(.SD), .SDcols = group2[[3]]]
  explanation_base2[, group4 := rowSums(.SD), .SDcols = group2[[4]]]
  explanation_base2[, group5 := rowSums(.SD), .SDcols = group2[[5]]]

  explanation_mat_post2 = as.matrix(explanation_base2[, ..group2_names])
  explanation_ranking_post2 = t(apply(-explanation_mat_post2, FUN = rank, 1))
  colnames(explanation_ranking_post2) = rank_group_names2
  explanation_base2 = cbind(explanation_base2, explanation_ranking_post2)

  pre_grouped = explanation_group2$dt[, ..group2_names]
  post_grouped = explanation_base2[, ..group2_names]

  pre_grouped_rank = explanation_group2$dt[, ..rank_group_names2]
  post_grouped_rank = explanation_base2[, ..rank_group_names2]
  results_group2[[length(results_group2) + 1]] = list(correlation = corr,
                                                      # pre_grouped = pre_grouped,
                                                      # post_grouped = post_grouped,
                                                      MAD = MAD(pre_grouped, post_grouped, weights = 1),
                                                      MDR = MDR(pre_grouped_rank, post_grouped_rank, weights = 1))

}

results_group_dt = rbindlist(results_group1)

library(ggplot2)

ggplot(data = results_group_dt) + geom_point(aes(x = correlation, y = MAD))
ggplot(data = results_group_dt) + geom_point(aes(x = correlation, y = MDR))

