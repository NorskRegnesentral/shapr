# This script is the function used in the script 2-cat-general_experiment-function.R

library(shapr)
library(MASS)
library(data.table)

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

general_experiment = function(No_test_obs,
                              corr,
                              seed = 1,
                              form,
                              model_function,
                              model_name){
  
  print(paste0("Correlation: ", corr))
  print(paste0("Now: ", Sys.Date()))
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
  set.seed(seed) # for fitting model
  x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
  
  dt <- NULL
  for(i in 1:dim){
    dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
  }
  
  # Get test data
  x_all_list <- list()
  for(i in 1:dim){
    x_all_list[[i]] <- 1:no_categories
  }
  x_all_dt <- do.call(CJ, x_all_list)
  
  # Mess up some of the levels of test data
  # all_levels <- list()
  # for(i in 1:3){
  #   all_levels[[i]] <- 1:no_categories
  # }
  # for(i in 4:6){
  #   all_levels[[i]] <- no_categories:1
  # }
  # for(i in 7:10){
  #   all_levels[[i]] <- -1*(1:no_categories)
  # }
  # all_levels_dt <- do.call(CJ, all_levels)
  
  # Get responses
  response_dt = data.table(x_all_dt)
  setnames(response_dt, names(response_dt), feat_names)
  # response_dt[, response := rowSums(.SD), .SDcols = feat_names]
  
  response_dt[, response := 1*feat_1_ + 2*feat_2_ + 3*feat_3_ + -1*feat_4_ + 
                -2*feat_5_ + -3*feat_6_ + 3*feat_7_ + 2*feat_8_ + 1*feat_9_ + -1*feat_10_]
  
  if(model_name == "experiment_lm2"){
    response_dt[feat_1_ == feat_2_, response := response + 3]
    response_dt[feat_3_ == feat_4_, response := response -1]
    response_dt[feat_5_ == feat_6_, response := response + 2.5]
    response_dt[feat_7_ == feat_8_, response := response + 1]
    response_dt[feat_9_ == feat_10_, response := response -1.5]
    
  } else if(model_name == "experiment_lm3"){
    response_dt[feat_1_ == feat_5_, response := response + 3]
    response_dt[feat_1_ == feat_7_, response := response -1]
    response_dt[feat_1_ == feat_9_, response := response + 2.5]
    response_dt[feat_3_ == feat_5_, response := response + 1]
    response_dt[feat_3_ == feat_7_, response := response + 0.5]
    response_dt[feat_3_ == feat_9_, response := response -1.5]
    response_dt[feat_5_ == feat_9_, response := response + 1]
  }
  
  # only need this if you "mess up the levels"
  # response_dt_new = data.table(x_test_dt)
  # setnames(response_dt_new, names(response_dt_new), feat_names)
  # response_dt_new[, response := response_dt$response]
  
  response_dt[, (feat_names) := lapply(.SD, as.factor), .SDcols = feat_names]
  
  # Calculate joint prob --- don't delete this!
  # set.seed(1)
  # time0 = Sys.time()
  # joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, feat_names, cutoff, response_dt)
  # print(Sys.time() - time0) # 2.15 min

  if(model_name == "experiment_lm1"){
    # fwrite(joint_prob_dt_list[[1]], "inst/paper_experiments/joint_prob_dt_experiment_lm1_new_response.csv")
    joint_prob_dt = fread("inst/paper_experiments/joint_prob_dt_experiment_lm1_new_response.csv")
  } else if(model_name == "experiment_lm2"){
    # fwrite(joint_prob_dt_list[[1]], "inst/paper_experiments/joint_prob_dt_experiment_lm2_new_response.csv")
    joint_prob_dt = fread("inst/paper_experiments/joint_prob_dt_experiment_lm2_new_response.csv")
  } else if(model_name == "experiment_lm3"){
    # fwrite(joint_prob_dt_list[[1]], "inst/paper_experiments/joint_prob_dt_experiment_lm3_new_response.csv")
    joint_prob_dt = fread("inst/paper_experiments/joint_prob_dt_experiment_lm3_new_response.csv")
  }
  joint_prob_dt[, (feat_names) := lapply(.SD, as.factor), .SDcols = feat_names] # fread converts evertything to numeric
  
  # Sample test data is too many
  set.seed(seed)
  sampled_rows <- sample(1:nrow(joint_prob_dt), size = No_test_obs, replace = FALSE, prob = joint_prob_dt$joint_prob)
  # set.seed(seed)
  # sampled_rows2 <- sample(1:nrow(joint_prob_dt), size = No_test_obs, replace = FALSE) # not weighted
  
  x_test_dt <- x_all_dt[sampled_rows,]
  
  #
  No_test_obs <- nrow(x_test_dt)
  dt <- data.table(rbind(dt, x_test_dt))
  setnames(dt, names(dt), feat_names)
  dt = dt[, lapply(.SD, as.factor)]
  
  # This messes up the ordering of train and test data so have to add id
  dt[, id := .I]
  dt = merge(dt, response_dt, by = feat_names)[order(id)]
  
  ## 4. Fit model
  model = model_function(form = form, train = dt[(1:No_train_obs)])
  # predict(model, response_dt[2,])
  
  ## 5. Initalize shapr object with trained model -- this is used for calculating true shapley
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[- (1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response)]

  ## 6. calculate the true shapley values
  p <- mean(y_train$response)

  x_test = merge(x_test, joint_prob_dt, by = feat_names) # add the joint_prob so that you can use as weights for MAD/MDR
  
  # Pre-grouping approach 1
  group1 <- list(group1 = 1:4,
                 group2 = 5:8,
                 group3 = 9:10)
  group1 = lapply(group1, function(x){feat_names[x]})

  explainer <- shapr(x_train, model, group = group1)
  print("shapr() for group A finished")
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "categorical",
    prediction_zero = p,
    joint_prob_dt = joint_prob_dt
  )
  print("explain() for group A finished")

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
  tmp[, response := dt[- (1:No_train_obs), "response"]]
  tmp[, joint_prob := x_test$joint_prob]
  
  fwrite(tmp, file = "inst/paper_experiments/results/cat-groupA_Shapley_values_new_response.csv", append = T)
  
  # Pre-grouping approach 2
  group2 <- list(group1 = 1:2,
                 group2 = 3:4,
                 group3 = 5:6,
                 group4 = 7:8,
                 group5 = 9:10
  )
  group2 = lapply(group2, function(x){feat_names[x]})
  
  explainer <- shapr(x_train, model, group = group2)
  print("shapr() for group B finished")
  explanation <- explain(
    x = x_test,
    explainer = explainer,
    approach = "categorical",
    prediction_zero = p,
    joint_prob_dt = joint_prob_dt
  )
  print("explain() for group B finished")
  
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
  tmp[, response := dt[- (1:No_train_obs), "response"]]
  tmp[, joint_prob := x_test$joint_prob]
  
  fwrite(tmp, file = "inst/paper_experiments/results/cat-groupB_Shapley_values_new_response.csv", append = T)
  
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
  print("explain() for full features finished")
  print(Sys.time() - time_start)
  

  # Saving full Shapley values
  tmp = data.table(explanation$dt)
  tmp[, correlation := corr]
  tmp[, pre_grouped := 0]
  tmp[, test_id := 1:.N, by = c("pre_grouped")]
  tmp[, model_type := model_name]
  tmp[, No_test_obs := No_test_obs]
  tmp[, response := dt[- (1:No_train_obs), "response"]]
  tmp[, joint_prob := x_test$joint_prob]
  
  fwrite(tmp, file = "inst/paper_experiments/results/cat-All_Shapley_values_lm_new_response.csv", append = T)

  print("Done")
}
