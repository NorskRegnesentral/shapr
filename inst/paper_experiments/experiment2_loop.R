# Read this: experiment 2 includes:
# 10 features, 2 different groupings
# correlations between features ranging from 0 to 0.9
# 2 linear models with interactions WITHIN groups
# that's why we need 2 models because the models differ depending on the group
# group1: {1, 2, 3, 4, 5}, {6, 7, 8}, {9, 10}. So we put interactions
# pairwise between 1:2, 1:3, 1:4, 1:5,
#                  2:3, 2:3, 2:4, 2:5,
#                  3:4, 3:5,
#                  6:7, 6:8,
#                  7:8,
#                  9:10,
#
# group2: {1, 2}, {3, 4}, {5, 6}, {7, 8}, {9, 10}. So we put interactions
# pairwise: 1:2, 3:4, 5:6, 7:8, 9:10

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
No_test_obs <- 5
No_train_obs <- 1000
#
results_group1 = NULL
results_group2 = NULL

corr_vec = c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
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


  ## 2. Define first model
  set.seed(seed)
  beta <- round(rnorm(44 + 1), 1)
  # -0.6  0.2 -0.8  1.6  0.3 -0.8  0.5  0.7  0.6 -0.3  1.5  0.4 -0.6 -2.2  1.1  0.0  0.0
  # [18]  0.9  0.8  0.6  0.9  0.8  0.1 -2.0  0.6 -0.1 -0.2 -1.5 -0.5  0.4  1.4 -0.1  0.4 -0.1
  # [35] -1.4 -0.4 -0.4 -0.1  1.1  0.8 -0.2 -0.3  0.7  0.6 -0.7
  make_response1 = function(X, beta){

    feat_1_ = X[, feat_1_]
    feat_2_ = X[, feat_2_]
    feat_3_ = X[, feat_3_]
    feat_4_ = X[, feat_4_]
    feat_5_ = X[, feat_5_]
    feat_6_ = X[, feat_6_]
    feat_7_ = X[, feat_7_]
    feat_8_ = X[, feat_8_]
    feat_9_ = X[, feat_9_]
    feat_10_ = X[, feat_10_]
    return (
      beta[1] +
        beta[2] * feat_1_ +
        beta[3] * feat_2_ +
        beta[4] * feat_3_ +
        beta[5] * feat_4_ +
        beta[6] * feat_5_ +
        beta[7] * feat_6_ +
        beta[8] * feat_7_ +
        beta[9] * feat_8_ +
        beta[10] * feat_9_ +
        beta[11] * feat_10_ +
        beta[12] * (feat_1_ * feat_2_) +
        beta[13] * (feat_1_ * feat_3_) +
        beta[14] * (feat_1_ * feat_4_) +
        beta[15] * (feat_1_ * feat_5_) +
        beta[16] * (feat_2_ * feat_3_) +
        beta[17] * (feat_2_ * feat_4_) +
        beta[18] * (feat_2_ * feat_5_) +
        beta[19] * (feat_3_ * feat_4_) +
        beta[20] * (feat_3_ * feat_5_) +
        beta[21] * (feat_4_ * feat_5_) +
        beta[22] * (feat_6_ * feat_7_) +
        beta[23] * (feat_6_ * feat_8_) +
        beta[24] * (feat_7_ * feat_8_) +
        beta[25] * (feat_9_ * feat_10_))

  }
  dt[, response := make_response1(.SD, beta = beta), .SDcols = feat_names]
  dt[, response := response + epsilon]
  dt[, mean_response := mean(response)]
  dt[, sd_response := sd(response)]
  dt[, response_stand := (response - mean_response) / sd_response]
  dt[, mean_response := NULL]
  dt[, sd_response := NULL]
  dt[, response := NULL]

  ## 4. Fit regression model 1
  form1 <- response_stand ~
    (feat_1_ * feat_2_) + (feat_1_ * feat_3_) +
    (feat_1_ * feat_4_) + (feat_1_ * feat_5_) + (feat_2_ * feat_3_) +
    (feat_2_ * feat_4_) + (feat_2_ * feat_5_) + (feat_3_ * feat_4_) +
    (feat_3_ * feat_5_) + (feat_4_ * feat_5_) + (feat_6_ * feat_7_) +
    (feat_6_ * feat_8_) + (feat_7_ * feat_8_) + feat_9_ * feat_10_

  model1 <- lm(formula = form1, data = dt[(1:No_train_obs)])

  ## 5. Initalize shapr object
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[- (1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response_stand)]

  p <- mean(y_train$response_stand)

  # Pre-grouping approach 1
  group1 <- list(group1 = 1:5, group2 = 6:8, group3 = 9:10)
  group1 = lapply(group1, function(x){names(x_test)[x]})

  explainer_group1 <- shapr(x_train, model1, group = group1)

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

  # Post-grouping approach - specificially for approach 1
  explainer <- shapr(x_train, model1)
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = 5000
  )

  # Compare group 1
  explanation_base1 = explanation$dt
  explanation_base1 = data.table(explanation_base1)
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

  # Pre-grouping approach 2
  # Define second model
  set.seed(seed)
  beta <- round(rnorm(15 + 1), 1)
  # -0.6  0.2 -0.8  1.6  0.3 -0.8  0.5  0.7  0.6 -0.3  1.5  0.4 -0.6 -2.2  1.1  0.0
  make_response2 = function(X, beta){

    feat_1_ = X[, feat_1_]
    feat_2_ = X[, feat_2_]
    feat_3_ = X[, feat_3_]
    feat_4_ = X[, feat_4_]
    feat_5_ = X[, feat_5_]
    feat_6_ = X[, feat_6_]
    feat_7_ = X[, feat_7_]
    feat_8_ = X[, feat_8_]
    feat_9_ = X[, feat_9_]
    feat_10_ = X[, feat_10_]
    return (
      beta[1] +
        beta[2] * feat_1_ +
        beta[3] * feat_2_ +
        beta[4] * feat_3_ +
        beta[5] * feat_4_ +
        beta[6] * feat_5_ +
        beta[7] * feat_6_ +
        beta[8] * feat_7_ +
        beta[9] * feat_8_ +
        beta[10] * feat_9_ +
        beta[11] * feat_10_ +
        beta[12] * (feat_1_ * feat_2_) +
        beta[13] * (feat_3_ * feat_4_) +
        beta[14] * (feat_5_ * feat_6_) +
        beta[15] * (feat_7_ * feat_8_) +
        beta[16] * (feat_9_ * feat_10_)
    )

  }
  dt[, response := make_response2(.SD, beta = beta), .SDcols = feat_names]
  dt[, response := response + epsilon]
  dt[, mean_response := mean(response)]
  dt[, sd_response := sd(response)]
  dt[, response_stand := (response - mean_response) / sd_response]
  dt[, mean_response := NULL]
  dt[, sd_response := NULL]
  dt[, response := NULL]

  ## 4. Fit regression model 2
  form2 <- response_stand ~
    (feat_1_ * feat_2_) +
    (feat_3_ * feat_4_) +
    (feat_5_ * feat_6_) +
    (feat_7_ * feat_8_) +
    (feat_9_ * feat_10_)

  model2 <- lm(formula = form2, data = dt[(1:No_train_obs)])

  ## 5. Initalize shapr object
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[- (1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response_stand)]

  p <- mean(y_train$response_stand)

  group2 <- list(group1 = 1:2,
                 group2 = 3:4,
                 group3 = 5:6,
                 group4 = 7:8,
                 group5 = 9:10)
  group2 = lapply(group2, function(x){names(x_test)[x]})

  explainer_group2 <- shapr(x_train, model2, group = group2)

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

  # Post-grouping approach  - specificially for approach 2
  explainer <- shapr(x_train, model2)
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = 5000
  )

  # compare group 2
  explanation_base2 = explanation$dt
  explanation_base2 = data.table(explanation_base2)
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

results_group_dt = cbind(rbindlist(results_group1), group = 1)
results_group_dt2 = cbind(rbindlist(results_group2), group = 2)

results = rbind(results_group_dt, results_group_dt2)
results$group = factor(results$group)

library(ggplot2)

ggplot(data = results) + geom_point(aes(x = correlation, y = MAD, col = group))
# ggplot(data = results_group_dt) + geom_point(aes(x = correlation, y = MDR))

