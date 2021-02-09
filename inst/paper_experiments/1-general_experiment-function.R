library(shapr)
library(MASS)
library(data.table)

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
                              form,
                              model_function,
                              model_name){
  
  print(paste0("Correlation: ", corr))
  # parameters
  dim <- 10
  no_categories <- 3
  mu <- rep(0, dim)
  cutoff <- cutoff <- c(-200, 0, 1, 200)
  No_train_obs <- 10000
  #
  Sample_test = TRUE # if this is TRUE, we sample from the test model.matrix. Otherwise we try all 3^6 comb
  #
  Sigma_diag <- 1
  Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
  for (i in 1:dim) {
    Sigma[i, i] <- Sigma_diag
  }

  ## 1. Simulate data
  feat_names <- paste0("feat_", 1:dim, "_")
  set.seed(seed) # we don't simulate test data since there are only 729 comb of features so 
  x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
  
  dt <- NULL
  for(i in 1:dim){
    dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
  }
  
  # Get test data
  x_test_list <- list()
  for(i in 1:dim){
    x_test_list[[i]] <- 1:no_categories
  }
  x_test_dt <- do.call(CJ, x_test_list)
  
  # Get responses
  response_dt = data.table(x_test_dt)
  setnames(response_dt, names(response_dt), feat_names)
  response_dt <- response_dt[, lapply(.SD, as.factor)]
  # response_dt[, response := c(rep(c(1, 2, 3, 4, 5), nrow(response_dt) / 5), 1, 2, 3, 4)]
  response_dt[, response := .I / 1000]
  # 6 = 729 - 1 sec
  # 7 = 2187
  # 8 = 6561
  # 9 = 19683
  # 10 = 59049
  
  # Sample test data if too many
  if(Sample_test){
    if(nrow(x_test_dt) > No_test_obs){
      sampled_rows <- sample(1:nrow(x_test_dt), size = No_test_obs, replace = FALSE)
      x_test_dt <- x_test_dt[sampled_rows, ]
    }
  }
  No_test_obs <- nrow(x_test_dt)
  dt <- data.table(rbind(dt, x_test_dt))
  setnames(dt, names(dt), feat_names)
  dt <- dt[, lapply(.SD, as.factor)]
  
  # This messes up the ordering of train and test data so have to add id
  dt[, id := .I]
  # dt = dt[response_dt, on = feat_names][order(id)]
  dt = merge(dt, response_dt, by = feat_names)[order(id)]
  
  ## 4. Fit model
  model = model_function(form = form, train = dt[(1:No_train_obs)])
  
  ## 5. Initalize shapr object with trained model -- this is used for calculating true shapley
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[- (1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response)]

  ## 6. calculate the true shapley values
  p <- mean(y_train$response)

  # Calculate joint prob
  # set.seed(1)
  # time0 = Sys.time()
  # joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, feat_names, cutoff, response_dt)
  # print(Sys.time() - time0) # 2.15 min
  # fwrite(joint_prob_dt_list[[1]], "inst/paper_experiments/joint_prob_dt.csv")
  joint_prob_dt = fread("inst/paper_experiments/joint_prob_dt.csv")
  joint_prob_dt[, (feat_names) := lapply(.SD, as.factor), .SDcols = feat_names]
  
  
  # Pre-grouping approach 1
  group1 <- list(group1 = 1:4,
                 group2 = 5:8,
                 group3 = 9:10)
  group1 = lapply(group1, function(x){feat_names[x]})

  explainer <- shapr(x_train, model, group = group1)
  print("shapr() for group 1 finished")
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "categorical",
    prediction_zero = p,
    joint_prob_dt = joint_prob_dt
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
  tmp[, test_id := 1:.N, by = c("pre_grouped")]
  tmp[, model_type := model_name]
  tmp[, grouping := "A"]
  tmp[, No_test_obs := No_test_obs]

  fwrite(tmp, file = "inst/paper_experiments/results/groupA_Shapley_values_categorical.csv", append = T)
  
  # Pre-grouping approach 2
  group2 <- list(group1 = 1:2,
                 group2 = 3:4,
                 group3 = 5:6,
                 group4 = 7:8,
                 group5 = 9:10
  )
  group2 = lapply(group2, function(x){feat_names[x]})
  
  explainer <- shapr(x_train, model, group = group2)
  print("shapr() for group 2 finished")
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "categorical",
    prediction_zero = p,
    joint_prob_dt = joint_prob_dt
  )
  print("explain() for group 2 finished")
  
  group2_names = copy(names(explainer$group))
  rank_group_names2 = paste0(group2_names, "_rank")
  explanation_group2_dt <- copy(explanation$dt)
  
  explanation_mat_pre = as.matrix(explanation_group2_dt[, ..group2_names])
  explanation_ranking_pre = t(apply(-explanation_mat_pre, FUN = rank, 1))
  colnames(explanation_ranking_pre) = rank_group_names2
  explanation_group2_dt = cbind(explanation_group2_dt, explanation_ranking_pre)
  
  tmp = data.table(explanation_group2_dt)
  tmp[, correlation := corr]
  tmp[, pre_grouped := 1]
  tmp[, test_id := 1:.N, by = c("pre_grouped")]
  tmp[, model_type := model_name]
  tmp[, grouping := "B"]
  tmp[, No_test_obs := No_test_obs]
  
  fwrite(tmp, file = "inst/paper_experiments/results/groupB_Shapley_values_categorical.csv", append = T)
  
  # Post-grouping approach
  explainer <- shapr(x_train, model)
  print("shapr() for full features finished")
  time_start = Sys.time()
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "categorical",
    prediction_zero = p,
    joint_prob_dt = joint_prob_dt
  )
  print(Sys.time() - time_start)
  print("explain() for full features finished")

  # Saving full Shapley values
  tmp = data.table(explanation$dt)
  tmp[, correlation := corr]
  tmp[, pre_grouped := 0]
  tmp[, test_id := 1:.N, by = c("pre_grouped")]
  tmp[, model_type := model_name]
  tmp[, No_test_obs := No_test_obs]

  fwrite(tmp, file = "inst/paper_experiments/results/All_Shapley_values_categorical_lm.csv", append = T)

  print("Done")
}
