library(shapr)
library(MASS)
library(data.table)

setwd("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/")

MAD <- function(pre_grouped, post_grouped, weights){
  mean(colSums((abs(pre_grouped - post_grouped)) * weights))
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped, weights){
  mean(colSums((abs(ranking_pre_grouped - ranking_post_grouped)) * weights))
}
get_model_specs.lm <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_list <- list()
  feature_list$labels <- names(attr(x$terms, "dataClasses")[-1])
  m <- length(feature_list$labels)

  feature_list$classes <- attr(x$terms, "dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_list)
}
assignInNamespace("get_model_specs.lm", get_model_specs.lm, "shapr")

get_model_specs.gam <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_list <- list()
  feature_list$labels <- names(attr(x$terms, "dataClasses")[-1])
  m <- length(feature_list$labels)

  feature_list$classes <- attr(x$terms, "dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_list)
}
assignInNamespace("get_model_specs.gam", get_model_specs.gam, "shapr")

general_experiment = function(No_test_obs,
                              corr,
                              seed = 1,
                              beta,
                              make_response_function,
                              model_function,
                              form,
                              model_name){
  print(paste0("Correlation: ", corr))
  # parameters
  dim <- 10
  mu <- rep(0, dim)
  set.seed(seed)
  No_train_obs <- 1000
  n_samples = 2000
  #
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
  dt[, response := make_response_function(.SD, beta = beta), .SDcols = feat_names]
  dt[, response := response + epsilon]

  ## 4. Fit regression model 1
  model = model_function(form = form, train = dt[(1:No_train_obs)])

  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[- (1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response)]

  p <- mean(y_train$response)

  # Pre-grouping approach 1
  group1 <- list(group1 = 1:4,
                 group2 = 5:8,
                 group3 = 9:10)
  group1 = lapply(group1, function(x){names(x_test)[x]})

  explainer <- shapr(x_train, model, group = group1)
  print("shapr() for group 1 finished")
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = n_samples
  )
  print("explain() for group 1 finished")

  group1_names = copy(names(explainer$group))
  rank_group_names1 = paste0(group1_names, "_rank")
  explanation_group1_dt <- copy(explanation$dt)

  explanation_mat_pre = as.matrix(explanation_group1_dt[, ..group1_names])
  explanation_ranking_pre = t(apply(-explanation_mat_pre, FUN = rank, 1))
  colnames(explanation_ranking_pre) = rank_group_names1
  explanation_group1_dt = cbind(explanation_group1_dt, explanation_ranking_pre)

  tmp = data.table(explanation_group1_dt)
  tmp[, correlation := corr]
  tmp[, pre_grouped := 1]
  tmp[, standardized := 0]
  tmp[, test_id := 1:.N, by = c("pre_grouped", "standardized")]
  tmp[, model_type := model_name]
  tmp[, grouping := "A"]
  tmp[, No_test_obs := No_test_obs]

  fwrite(tmp, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/paper_experiments/results/group1_Shapley_values_GAM.csv", append = T)

  # Pre-grouping approach 2
  group2 <- list(group1 = 1:2,
                 group2 = 3:4,
                 group3 = 5:6,
                 group4 = 7:8,
                 group5 = 9:10)
  group2 = lapply(group2, function(x){names(x_test)[x]})

  explainer <- shapr(x_train, model, group = group2)
  print("shapr() for group 2 finished")
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = n_samples
  )
  print("explain() for group 2 finished")

  group2_names = copy(names(explainer$group))
  rank_group_names2 = paste0(group2_names, "_rank")
  explanation_group2_dt <- copy(explanation$dt)

  explanation_mat_pre2 = as.matrix(explanation_group2_dt[, ..group2_names])
  explanation_ranking_pre2 = t(apply(-explanation_mat_pre2, FUN = rank, 1))
  colnames(explanation_ranking_pre2) = rank_group_names2
  explanation_group2_dt = cbind(explanation_group2_dt, explanation_ranking_pre2)

  tmp = data.table(explanation_group2_dt)
  tmp[, correlation := corr]
  tmp[, pre_grouped := 1]
  tmp[, standardized := 0]
  tmp[, test_id := 1:.N, by = c("pre_grouped", "standardized")]
  tmp[, model_type := model_name]
  tmp[, grouping := "B"]
  tmp[, No_test_obs := No_test_obs]

  fwrite(tmp, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/paper_experiments/results/group2_Shapley_values_GAM.csv", append = T)

  # Post-grouping approach
  explainer <- shapr(x_train, model)
  print("shapr() for full features finished")
  time_start = Sys.time()
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "gaussian",
    prediction_zero = p,
    mu = mu,
    cov_mat = Sigma,
    n_samples = n_samples
  )
  print(Sys.time() - time_start)
  print("explain() for full features finished")

  # Saving full Shapley values
  tmp = data.table(explanation$dt)
  tmp[, correlation := corr]
  tmp[, pre_grouped := 0]
  tmp[, standardized := 0]
  tmp[, test_id := 1:.N, by = c("pre_grouped", "standardized")]
  tmp[, model_type := model_name]
  tmp[, No_test_obs := No_test_obs]

  fwrite(tmp, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/paper_experiments/results/All_Shapley_values_GAM.csv", append = T)

  print(paste0("object size: ", pryr::object_size(explanation) / 10^6, " MB"))

  # Compare group 1
  explanation_base1 = copy(explanation$dt)
  explanation_base1 = data.table(explanation_base1)
  explanation_base1[, group1 := rowSums(.SD), .SDcols = group1[[1]]]
  explanation_base1[, group2 := rowSums(.SD), .SDcols = group1[[2]]]
  explanation_base1[, group3 := rowSums(.SD), .SDcols = group1[[3]]]

  explanation_mat_post1 = as.matrix(explanation_base1[, ..group1_names])
  explanation_ranking_post1 = t(apply(-explanation_mat_post1, FUN = rank, 1))
  colnames(explanation_ranking_post1) = rank_group_names1
  explanation_base1 = cbind(explanation_base1, explanation_ranking_post1)

  pre_grouped = explanation_group1_dt[, ..group1_names]

  mean_pre_grouped = matrix(apply(pre_grouped, 2, mean), ncol = ncol(pre_grouped), nrow = nrow(pre_grouped), byrow = T)
  sd_pre_grouped = sd(as.matrix(pre_grouped))

  pre_grouped_stand = (pre_grouped - mean_pre_grouped) / sd_pre_grouped

  post_grouped = explanation_base1[, ..group1_names]
  post_grouped_stand = (post_grouped - mean_pre_grouped) / sd_pre_grouped

  pre_grouped_rank = explanation_group1_dt[, ..rank_group_names1]
  post_grouped_rank = explanation_base1[, ..rank_group_names1]

  results_csvPre = data.frame(correlation = corr,
                              pre_grouped = 1,
                              standardized = c(rep(0, nrow(pre_grouped)), rep(1, nrow(pre_grouped_stand))),
                              rbind(pre_grouped, pre_grouped_stand),
                              pre_grouped_rank)

  results_csvPost = data.frame(correlation = corr,
                               pre_grouped = 0,
                               standardized = c(rep(0, nrow(post_grouped)), rep(1, nrow(post_grouped_stand))),
                               rbind(post_grouped, post_grouped_stand),
                               post_grouped_rank)
  results_csv1 = data.table(rbind(results_csvPre, results_csvPost))
  results_csv1[, test_id := 1:.N, by = c("pre_grouped", "standardized")]
  results_csv1[, model_type := model_name]
  results_csv1[, grouping := "A"]
  results_csv1[, No_test_obs := No_test_obs]
  results_csv1[, MAD := MAD(pre_grouped_stand, post_grouped_stand, weights = 1)]
  results_csv1[, MDR := MDR(pre_grouped_rank, post_grouped_rank, weights = 1)]
  # print(MAD(pre_grouped_stand, post_grouped_stand, weights = 1))

  fwrite(tmp, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/paper_experiments/results/results_groupA_GAM.csv", append = T)

  # Compare group 2
  explanation_base2 = copy(explanation$dt)
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

  pre_grouped = explanation_group2_dt[, ..group2_names]

  mean_pre_grouped = matrix(apply(pre_grouped, 2, mean), ncol = ncol(pre_grouped), nrow = nrow(pre_grouped), byrow = T)
  sd_pre_grouped = sd(as.matrix(pre_grouped))

  pre_grouped_stand = (pre_grouped - mean_pre_grouped) / sd_pre_grouped

  post_grouped = explanation_base2[, ..group2_names]
  post_grouped_stand = (post_grouped - mean_pre_grouped) / sd_pre_grouped

  pre_grouped_rank = explanation_group2_dt[, ..rank_group_names2]
  post_grouped_rank = explanation_base2[, ..rank_group_names2]

  results_csvPre = data.frame(correlation = corr,
                              pre_grouped = 1,
                              standardized = c(rep(0, nrow(pre_grouped)), rep(1, nrow(pre_grouped_stand))),
                              rbind(pre_grouped, pre_grouped_stand),
                              pre_grouped_rank)

  results_csvPost = data.frame(correlation = corr,
                               pre_grouped = 0,
                               standardized = c(rep(0, nrow(post_grouped)), rep(1, nrow(post_grouped_stand))),
                               rbind(post_grouped, post_grouped_stand),
                               post_grouped_rank)
  results_csv1 = data.table(rbind(results_csvPre, results_csvPost))
  results_csv1[, test_id := 1:.N, by = c("pre_grouped", "standardized")]
  results_csv1[, model_type := model_name]
  results_csv1[, grouping := "B"]
  results_csv1[, No_test_obs := No_test_obs]
  results_csv1[, MAD := MAD(pre_grouped_stand, post_grouped_stand, weights = 1)]
  results_csv1[, MDR := MDR(pre_grouped_rank, post_grouped_rank, weights = 1)]
  # print(MAD(pre_grouped_stand, post_grouped_stand, weights = 1))

  fwrite(tmp, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/paper_experiments/results/results_groupB_GAM.csv", append = T)
  print("Done")
}

