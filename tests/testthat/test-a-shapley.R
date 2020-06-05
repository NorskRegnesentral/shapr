library(testthat)
library(shapr)

testthat::context("test-shapley.R")

RNGversion(vstr = "3.5.0")

testthat::test_that("Basic test functions in shapley.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  x_train <- tail(Boston[, x_var], 50)

  # Load premade lm model. Path needs to be relative to testthat directory in the package
  model <- readRDS("model_objects/lm_model_object.rds")

  # Prepare the data for explanation
  explainer <- shapr(x_train, model)

  testthat::expect_known_value(explainer, file = "test_objects/shapley_explainer_obj.rds")
})

test_that("Basic test functions for grouping in shapley.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  x_train <- tail(Boston[, x_var], 50)

  # Load premade lm model. Path needs to be relative to testthat directory in the package
  model <- readRDS("model_objects/lm_model_object.rds")

  group1 <- list(c(1,3),
                 c(2,4))

  group1_names = lapply(group1, function(x){x_var[x]})


  group2 <- list(c(1),
                 c(2),
                 c(3),
                 c(4))

  group2_names = lapply(group2, function(x){x_var[x]})

  # Prepare the data for explanation
  explainer1 <- shapr(x_train, model, group = group1_names)
  explainer2 <- shapr(x_train, model, group = group2_names)

  expect_known_value(explainer1, file = "test_objects/shapley_explainer_group1_obj.rds")
  expect_known_value(explainer2, file = "test_objects/shapley_explainer_group2_obj.rds")
})

testthat::test_that("Testing data input to shapr in shapley.R", {

  data("Boston", package = "MASS")

  x_var <- c("lstat", "rm", "dis", "indus")
  x_var_sub <- x_var[1:2]
  not_x_var <- "crim"
  not_even_var <- "not_a_column_name"

  x_train <- as.matrix(tail(Boston[, x_var], -6))
  xy_train_full_df <- tail(Boston[, ], -6)
  xy_train_missing_lstat_df <- xy_train_full_df[, !(colnames(xy_train_full_df) == "lstat")]
  xy_train_full_df_no_colnames <- xy_train_full_df
  colnames(xy_train_full_df_no_colnames) <- NULL

  # Fitting models
  formula <- as.formula(paste0("medv ~ ", paste0(x_var, collapse = "+")))

  l <- list(
    xgboost::xgboost(
      data = x_train,
      label = tail(Boston[, "medv"], -6),
      nround = 3,
      verbose = FALSE
    ),
    lm(
      formula = formula,
      data = xy_train_full_df
    ),
    ranger::ranger(
      formula = formula,
      data = xy_train_full_df,
      num.trees = 50
    )
  )

  for (i in seq_along(l)) {

    # Expect silent
    testthat::expect_silent(shapr(xy_train_full_df, l[[i]]))

    # Expect message that feature_labels is ignored
    testthat::expect_message(shapr(xy_train_full_df, l[[i]], feature_labels = x_var_sub))
    testthat::expect_message(shapr(xy_train_full_df, l[[i]], feature_labels = x_var))

    # Expect error, giving error message that indicates that x misses columns used by the model
    testthat::expect_error(shapr(xy_train_missing_lstat_df, l[[i]]))

    # Expect error when x_train don't have column names
    testthat::expect_error(shapr(xy_train_full_df_no_colnames, l[[i]], feature_labels = x_var_sub))
  }
})

test_that("Testing data input to shapr for grouping in shapley.R", {

  data("Boston", package = "MASS")

  x_var <- c("lstat", "rm", "dis", "indus")
  x_var_sub <- x_var[1:2]
  not_x_var <- "crim"
  not_even_var <- "not_a_column_name"

  x_train <- as.matrix(tail(Boston[, x_var], -6))
  xy_train_full_df <- tail(Boston[, ], -6)
  xy_train_missing_lstat_df <- xy_train_full_df[, !(colnames(xy_train_full_df) == "lstat")]
  xy_train_full_df_no_colnames <- xy_train_full_df
  colnames(xy_train_full_df_no_colnames) <- NULL

  group1_names <- list(c("lstat","dis"),
                       c("rm", "indus"))

  group11_names <- list(c("dis"),
                        c("rm", "indus"))


  group2_names <- list(c("lstat"),
                       c("dis"),
                       c("rm"),
                       c("indus"))

  group3_names <- list(c("rm", not_even_var), c("lstat", "dis")) # includes a non-existing feature

  group4_names <- list(c("rm", "rm", "indus"), c("lstat", "dis")) # includes a repeating feature

  group5_names <- list(c("rm", not_x_var), c("lstat", "dis")) # includes a feature not in model

  group6_names <- list(c(2, 1), c("lstat", "dis")) # includes non-character in the list

  group7_names <- list(c("lstat", "rm"), c("rm", "dis", "indus")) # includes a feature in both groups


  # Fitting models
  formula <- as.formula(paste0("medv ~ ", paste0(x_var, collapse = "+")))

  l <- list(
    xgboost::xgboost(
      data = x_train,
      label = tail(Boston[, "medv"], -6),
      nround = 3,
      verbose = FALSE
    ),
    lm(formula = formula,
       data = xy_train_full_df
    ),
    ranger::ranger(
      formula = formula,
      data = xy_train_full_df,
      num.trees = 50
    )
  )


  for (i in seq_along(l)) {
    # Expect silent
    expect_silent(shapr(x = xy_train_full_df, model = l[[i]], group = group1_names))

    # Expect error that group includes a feature that is not in x or model
    expect_error(shapr(x = xy_train_full_df, model = l[[i]], group = group3_names))

    # Expect message that feature_labels is ignored
    expect_message(shapr(x = xy_train_full_df, model = l[[i]], feature_labels = x_var, group = group1_names))

    # Expect error that indicates that x misses columns used by the model
    expect_error(shapr(x = xy_train_missing_lstat_df, model = l[[i]], group = group11_names))

    # Expect error when x_train doesn't have column names
    expect_error(shapr(xy_train_full_df_no_colnames, l[[i]], group = group1_names))

    # Expect error when group is not made up of characters
    expect_error(shapr(x_train, l[[i]], group = group6_names))

    # Expect error when group includes a duplicate variable
    expect_error(shapr(x_train, l[[i]], group = group4_names))

    # Expect error when feature appears in more than one group
    expect_error(shapr(x_train, l[[i]], group = group7_names))
}

  formula2 <- as.formula(paste0("medv ~ ", paste0(x_var_sub, collapse = "+")))
  model_sub <- lm(formula = formula2, data = xy_train_full_df)

  # Expect error when group includes a variable in the data, but not in model
  expect_error(shapr(x_train, model_sub, group = group1_names))

})
