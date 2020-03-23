library(testthat)
library(shapr)

context("test-explanation.R")

# For using same Random numer generator as CircelCI (R version 3.5.x)
RNGversion(vstr = "3.5.0")

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
  for (i in 1:length(ex_list)) {
    expect_equal(ex_list[[i]]$dt, fixed_explain_obj_list[[i]]$dt)
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

  # Setup for training data and explainer object
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  # Training data
  x_train <- as.matrix(tail(Boston[, x_var], -6))
  y_train <- tail(Boston[, y_var], -6)
  xy_train_full_df <- tail(Boston[, ], -6)

  # Test data
  x_test <- as.matrix(head(Boston[, x_var], 6))
  x_test_full <- as.matrix(head(Boston[, ], 6))
  x_test_reordered <- as.matrix(head(Boston[, rev(x_var)], 6))
  xy_test_full_df <- head(Boston[, ], 6)
  xy_test_missing_lstat_df <- xy_test_full_df[, !(colnames(xy_test_full_df) == "lstat")]
  xy_test_full_df_no_colnames <- xy_test_full_df
  colnames(xy_test_full_df_no_colnames) <- NULL

  # Fitting models
  formula <- as.formula(paste0("medv ~ ", paste0(x_var, collapse = "+")))
  model1 <- xgboost::xgboost(
    data = x_train,
    label = y_train,
    nround = 5,
    verbose = FALSE
  )

  model2 <- lm(
    formula = formula,
    data = xy_train_full_df
  )

  model3 <- ranger::ranger(
    formula = formula,
    data = xy_train_full_df,
    num.trees = 50
  )

  p0 <- mean(y_train)

  # Get explainer objects
  all_explainers <- lapply(list(model1, model2, model3), shapr, x = x_train)

  # Test data
  all_test_data <- list(
    x_test,
    x_test_reordered,
    x_test_full
  )

  # Expect silent for explainer 1, using correct, reordered and full data set, then identical results
  l <- list()
  for (i in seq_along(all_test_data)) {
    l[[i]] <- expect_silent(
      explain(
        all_test_data[[i]],
        all_explainers[[1]],
        approach = "empirical",
        prediction_zero = p0,
        n_samples = 1e2
      )
    )
  }
  for (i in 2:length(l)) {
    expect_equal(l[[i - 1]], l[[i]])
  }

  # Expect silent for explainer 2, using correct, reordered and bigger data set, then identical results
  l <- list()
  for (i in seq_along(all_test_data)) {
    l[[i]] <- expect_silent(
      explain(
        all_test_data[[i]],
        all_explainers[[2]],
        approach = "empirical",
        prediction_zero = p0,
        n_samples = 1e2
      )
    )
  }
  for (i in 2:length(l)) {
    expect_equal(l[[i - 1]], l[[i]])
  }

  # Expect silent for explainer 3, using correct, reordered and bigger data set, then identical results
  l <- list()
  for (i in seq_along(all_test_data)) {
    l[[i]] <- expect_silent(
      explain(
        all_test_data[[i]],
        all_explainers[[3]],
        approach = "empirical",
        prediction_zero = p0,
        n_samples = 1e2
      )
    )
  }
  for (i in 2:length(l)) {
    expect_equal(l[[i - 1]], l[[i]])
  }

  for (i in seq_along(all_explainers)) {

    # Expect error when test data misses used variable
    expect_error(
      explain(
        xy_test_missing_lstat_df,
        all_explainers[[i]],
        approach = "empirical",
        prediction_zero = p0,
        n_samples = 1e2
      )
    )

    # Expect error when test data misses column names
    expect_error(
      explain(
        xy_test_full_df_no_colnames,
        all_explainers[[i]],
        approach = "empirical",
        prediction_zero = p0,
        n_samples = 1e2
      )
    )
  }
})
