library(testthat)
library(shapr)

context("test-explanation.R")

# For using same Random numer generator as CircelCI (R version 3.5.x)
if (as.numeric(version$minor) >= 6.0) RNGkind(sample.kind = "Rounding")

test_that("Test functions in explanation.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  y_train <- tail(Boston[, y_var], 50)
  x_test <- as.matrix(head(Boston[, x_var], 2))

  # Prepare the data for explanation. Path needs to be relative to testthat directory in the package
  explainer <- readRDS(file = "test_objects/shapley_explainer_obj.rds")

  # Creating list with lots of different explainer objects
  p0 <- mean(y_train)
  ex_list <- list()

  # Ex 1: Explain predictions (gaussian)
  ex_list[[1]] <- explain(x_test, explainer, approach = "gaussian", prediction_zero = p0)

  # Ex 2: Explain predictions (copula)
  ex_list[[2]] <- explain(x_test, explainer, approach = "copula", prediction_zero = p0)

  # Ex 3: Explain predictions (empirical, independence):
  ex_list[[3]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "independence")

  # Ex 4: Explain predictions (empirical, fixed sigma)
  ex_list[[4]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "fixed_sigma")

  # Ex 5: Explain predictions (empirical, AICc)
  ex_list[[5]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "AICc_each_k")

  # Ex 6: Explain predictions (empirical, AICc full)
  ex_list[[6]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "AICc_full")

  # Ex 7: Explain combined - empirical and gaussian
  ex_list[[7]] <- explain(x_test, explainer, approach = c("empirical", rep("gaussian", 3)), prediction_zero = p0)

  # Ex 8: Explain combined II - all gaussian
  ex_list[[8]] <- explain(x_test, explainer, approach = c(rep("gaussian", 4)), prediction_zero = p0)

  # Ex 9: Explain combined III - all copula
  ex_list[[9]] <- explain(x_test, explainer, approach = rep("copula", 4), prediction_zero = p0)

  # Ex 10: gaussian and copula XX (works with seed)
  approach <- c(rep("gaussian", 2), rep("copula", 2))
  ex_list[[10]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 11: empirical and gaussian
  approach <- c(rep("empirical", 2), rep("gaussian", 2))
  ex_list[[11]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 12: empirical and copula
  approach <- c(rep("empirical", 2), rep("copula", 2))
  ex_list[[12]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 13: copula and empirical XX (works now)
  approach <- c(rep("copula", 2), rep("empirical", 2))
  ex_list[[13]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 14: gaussian and copula XX (works with seed)
  approach <- c(rep("gaussian", 1), rep("copula", 3))
  ex_list[[14]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 15: empirical and copula
  approach <- c(rep("empirical", 1), rep("copula", 3))
  ex_list[[15]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 16: gaussian and empirical XX (works now)
  approach <- c(rep("gaussian", 1), rep("empirical", 3))
  ex_list[[16]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 17: gaussian and empirical XX (works now!)
  approach <- c(rep("gaussian", 2), rep("empirical", 2))
  ex_list[[17]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 8: Explain combined II - all empirical
  approach <- c(rep("empirical", 4))
  ex_list[[18]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Checking that all explain objects produce the same as before
  expect_known_value(ex_list, file = "test_objects/explanation_explain_obj_list.rds")

  ### Additional test that only the produced shapley values are the same as before
  fixed_explain_obj_list <- readRDS("test_objects/explanation_explain_obj_list_fixed.rds")
  for (i in 1:length(ex_list)){
    expect_equal(ex_list[[i]]$dt,fixed_explain_obj_list[[i]]$dt)
  }


  # Checks that an error is returned
  expect_error(
    explain(1, explainer, approach = "gaussian", prediction_zero = p0)
  )
  expect_error(
    explain(list(), explainer, approach = "gaussian", prediction_zero = p0)
  )
  expect_error(
    explain(x_test, explainer, approach = "Gaussian", prediction_zero = p0)
  )
  expect_error(
    explain(x_test, explainer, approach = rep("gaussian", ncol(x_test) + 1), prediction_zero = p0)
  )
})



test_that("Testing data input to explain in explanation.R", {

  data("Boston", package = "MASS")

  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"
  x_var_sub <- x_var[1:2]
  not_x_var <- "crim"
  not_even_var <- "not_a_column_name"

  x_train <- as.matrix(tail(Boston[, x_var], -6))
  y_train <- tail(Boston[, y_var], -6)

  xy_train_full_df <- tail(Boston[, ], -6)
  xy_train_missing_lstat_df <- xy_train_full_df[,!(colnames(xy_train_full_df) == "lstat")]


  x_test <- as.matrix(head(Boston[, x_var], 6))
  x_test_full <- as.matrix(head(Boston[, ], 6))
  x_test_reordered <- as.matrix(head(Boston[, rev(x_var)], 6))
  xy_test_full_df <- head(Boston[, ], 6)
  xy_test_missing_lstat_df <- xy_test_full_df[,!(colnames(xy_test_full_df) == "lstat")]

  # Fitting models
  model1 <- xgboost::xgboost(
    data = x_train,
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  formula <- as.formula(paste0("medv ~ ",paste0(x_var,collapse="+")))
  model2 <- lm(formula = formula,
               data = xy_train_full_df)
  model3 <- ranger::ranger(formula = formula,
                           data = xy_train_full_df,
                           num.trees = 50)

  # Just making up a fictive model class
  model4 <- "cumstom_testmodel"
  class(model4) = "testclass"

  # Create custom function of model_type for caret
  model_type.testclass <- function(x) {
    "regression"
  }

  # Create custom function of predict_model for caret
  predict_model.testclass <- function(x, newdata) {
    if (!any(colnames(newdata)=="lstat")){
      stop("lstat not in newdata")
    }
    newdata[,which(colnames(newdata)=="lstat")]
  }

  #### Running tests ####
  p0 <- mean(y_train)

  # expect_success
  explainer1 <- shapr(x_train, model1)
  explainer2 <- shapr(xy_train_full_df, model2)
  explainer3<- shapr(xy_train_full_df, model3)
  explainer4 <- shapr(xy_train_full_df, model4,feature_labels = x_var)


  # expect silent for explainer 1, using correct data, reordered data and bigger data set, then identical results
  expect_silent(explanation1_success <- explain(x_test,
                                                explainer1,
                                                approach = "empirical",
                                                prediction_zero = p0))
  expect_silent(explanation1_success2 <- explain(x_test_reordered,
                                                 explainer1,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_silent(explanation1_success3 <- explain(x_test_full,
                                                 explainer1,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_equal(explanation1_success,explanation1_success2) # FAILS due to first lines in explain_x_test. Remove those
  expect_equal(explanation1_success2,explanation1_success3)

  # expect silent for explainer 2, using correct data, reordered data and bigger data set, then identical results
  expect_silent(explanation2_success <- explain(x_test,
                                                explainer2,
                                                approach = "empirical",
                                                prediction_zero = p0))
  expect_silent(explanation2_success2 <- explain(x_test_reordered,
                                                 explainer2,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_silent(explanation2_success3 <- explain(x_test_full,
                                                 explainer2,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_equal(explanation2_success,explanation2_success2) # FAILS due to first lines in explain_x_test. Remove those
  expect_equal(explanation2_success2,explanation2_success3)

  # expect silent for explainer 1, using correct data, reordered data and bigger data set, then identical results
  expect_silent(explanation3_success <- explain(x_test,
                                                explainer3,
                                                approach = "empirical",
                                                prediction_zero = p0))
  expect_silent(explanation3_success2 <- explain(x_test_reordered,
                                                 explainer3,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_silent(explanation3_success3 <- explain(x_test_full,
                                                 explainer3,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_equal(explanation3_success,explanation3_success2) # FAILS due to first lines in explain_x_test. Remove those
  expect_equal(explanation3_success2,explanation3_success3)

  # expect silent for explainer 1, using correct data, reordered data and bigger data set, then identical results
  expect_silent(explanation4_success <- explain(x_test,
                                                explainer4,
                                                approach = "empirical",
                                                prediction_zero = p0))
  expect_silent(explanation4_success2 <- explain(x_test_reordered,
                                                 explainer4,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_silent(explanation4_success3 <- explain(x_test_full,
                                                 explainer4,
                                                 approach = "empirical",
                                                 prediction_zero = p0))
  expect_equal(explanation4_success,explanation4_success2) # FAILS due to first lines in explain_x_test. Remove those
  expect_equal(explanation4_success2,explanation4_success3)

  # expect error when test data misses used variable
  expect_error(explanation1_error <- explain(xy_test_missing_lstat_df,
                                             explainer1,
                                             approach = "empirical",
                                             prediction_zero = p0))
  expect_error(explanation2_error <- explain(xy_test_missing_lstat_df,
                                             explainer2,
                                             approach = "empirical",
                                             prediction_zero = p0))
  expect_error(explanation3_error <- explain(xy_test_missing_lstat_df,
                                             explainer3,
                                             approach = "empirical",
                                             prediction_zero = p0))
  expect_error(explanation4_error <- explain(xy_test_missing_lstat_df,
                                             explainer4,
                                             approach = "empirical",
                                             prediction_zero = p0))

})


