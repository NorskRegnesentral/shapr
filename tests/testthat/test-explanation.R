library(testthat)
library(shapr)

context("test-explanation.R")

# For using same Random numer generator as CircelCI (R version 3.5.x)
RNGversion(vstr = "3.5.0")

test_that("Test get_list_approaches", {

  m <- 4
  n_features <- c(0, 1, 1, 1, 2, 2, 2, 3)
  approach <- c("gaussian", "copula", "copula")
  l <- get_list_approaches(n_features, approach)

  expect_true(is.list(l))
  expect_equal(names(l), c("gaussian", "copula"))
  expect_equal(l$gaussian, 1:4)
  expect_equal(l$copula, 5:8)
})

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

  # Ex 18: Explain combined II - all empirical
  approach <- c(rep("empirical", 4))
  ex_list[[18]] <- explain(x_test, explainer, approach = approach, prediction_zero = p0)

  # Ex 19: Explain predictions (ctree, sample = FALSE, default parameters)
  ex_list[[19]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE)

  # Ex 20: Explain predictions (ctree, sample = TRUE, default parameters)
  ex_list[[20]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE)

  # Ex 21: Explain predictions (ctree, sample = FALSE, other ctree parameters)
  ex_list[[21]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,
                           mincriterion = 0.9, minsplit = 20, minbucket = 25)

  # Ex 22: Explain predictions (ctree, sample = TRUE, other ctree parameters)
  ex_list[[22]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
                           mincriterion = 0.9, minsplit = 20, minbucket = 25)

  # Ex 23: Explain combined - ctree and gaussian, sample = FALSE
  ex_list[[23]] <- explain(x_test, explainer, approach = c("ctree", rep("gaussian", 3)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 24: Explain combined II - ctree and gaussian, sample = FALSE
  ex_list[[24]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("gaussian", 2)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 25: Explain combined III - ctree and gaussian, sample = FALSE
  ex_list[[25]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("gaussian", 1)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 26: Explain combined IV - ctree all, sample = FALSE
  ex_list[[26]] <- explain(x_test, explainer, approach = c(rep("ctree", 4)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 27: Explain combined - ctree and empirical, sample = FALSE
  ex_list[[27]] <- explain(x_test, explainer, approach = c("ctree", rep("empirical", 3)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 28: Explain combined II - ctree and empirical, sample = FALSE
  ex_list[[28]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("empirical", 2)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 29: Explain combined III - ctree and empirical, sample = FALSE
  ex_list[[29]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("empirical", 1)),
                           prediction_zero = p0, sample = FALSE)

  # Ex 30: Explain combined - ctree and gaussian, sample = TRUE
  ex_list[[30]] <- explain(x_test, explainer, approach = c("ctree", rep("gaussian", 3)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 31: Explain combined II - ctree and gaussian, sample = TRUE
  ex_list[[31]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("gaussian", 2)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 32: Explain combined III - ctree and gaussian, sample = TRUE
  ex_list[[32]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("gaussian", 1)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 33: Explain combined IV - ctree all, sample = TRUE
  ex_list[[33]] <- explain(x_test, explainer, approach = c(rep("ctree", 4)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 34: Explain combined - ctree and empirical, sample = TRUE
  ex_list[[34]] <- explain(x_test, explainer, approach = c("ctree", rep("empirical", 3)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 35: Explain combined II - ctree and empirical, sample = TRUE
  ex_list[[35]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("empirical", 2)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 36: Explain combined III - ctree and empirical, sample = TRUE
  ex_list[[36]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("empirical", 1)),
                           prediction_zero = p0, sample = TRUE)

  # Ex 37: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[37]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
                           mincriterion = c(0.05, 0.05, 0.95, 0.95))

  # Ex 38: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[38]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
                           mincriterion = rep(0.95, 4))

  # Ex 38: Test that ctree with mincriterion equal to same probability four times gives the same as only passing one
  # probability to mincriterion
  expect_equal(
    (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
             mincriterion = rep(0.95, 4)))$dt,
    (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
             mincriterion = 0.95))$dt
  )


  # Ex 39: Test that ctree with the same mincriterion repeated four times is the same as passing mincriterion only once
  expect_equal(
    (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,
             mincriterion = c(rep(0.95, 2), rep(0.95, 2))))$dt,
    (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,
             mincriterion = 0.95))$dt
  )

  # Checking that explanations with different paralellizations gives the same result (only unix systems!)

  if (.Platform$OS.type == "unix") {
    explain_base_nosample <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE)

    multicore <- 2

    expect_equal(
      explain_base_nosample,
      explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,
              mc_cores = multicore)
    )

    expect_equal(
      explain_base_nosample,
      explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,
              mc_cores_create_ctree = 1, mc_cores_sample_ctree = multicore)
    )

    expect_equal(
      explain_base_nosample,
      explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,
              mc_cores_create_ctree = multicore, mc_cores_sample_ctree = 1)
    )

    explain_base_sample <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE)

    # Seed consistent when only paralellizing create_ctree, and not sample_ctree
    expect_equal(
      explain_base_sample,
      explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
              mc_cores_create_ctree = multicore, mc_cores_sample_ctree = 1)
    )

    # Seed consistent, when run twice with same seed
    expect_equal(
      explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
              mc_cores = multicore),
      explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE,
              mc_cores = multicore)
    )
  }

  # Checking that all explain objects produce the same as before
  expect_known_value(ex_list, file = "test_objects/explanation_explain_obj_list.rds",
                     update = F)

  ### Additional test to test that only the produced shapley values are the same as before
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

  # explainer 1
  # Expect message due to no label/factor checking
  l <- list()
  l[[1]] <- expect_silent(
    explain(
      all_test_data[[1]],
      all_explainers[[1]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  # Expect message due to no label/factor checking
  l[[2]] <- expect_silent(
    explain(
      all_test_data[[2]],
      all_explainers[[1]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  # Expect message due to removal of data
  l[[3]] <- expect_message(
    explain(
      all_test_data[[3]],
      all_explainers[[1]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  for (i in 2:length(l)) {
    expect_equal(l[[i - 1]], l[[i]])
  }

  # explainer 2
  # Expect silent
  l <- list()
  l[[1]] <- expect_silent(
    explain(
      all_test_data[[1]],
      all_explainers[[2]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  # Expect silent
  l[[2]] <- expect_silent(
    explain(
      all_test_data[[2]],
      all_explainers[[2]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  # Expect message due to removal of data
  l[[3]] <- expect_message(
    explain(
      all_test_data[[3]],
      all_explainers[[2]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  for (i in 2:length(l)) {
    expect_equal(l[[i - 1]], l[[i]])
  }

  # explainer 3
  # Expect silent
  l <- list()
  l[[1]] <- expect_silent(
    explain(
      all_test_data[[1]],
      all_explainers[[3]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  # Expect silent
  l[[2]] <- expect_silent(
    explain(
      all_test_data[[2]],
      all_explainers[[3]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
  # Expect message due removal of data
  l[[3]] <- expect_message(
    explain(
      all_test_data[[3]],
      all_explainers[[3]],
      approach = "empirical",
      prediction_zero = p0,
      n_samples = 1e2
    )
  )
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
