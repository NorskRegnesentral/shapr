library(testthat)
library(shapr)

context("test-shapley.R")

RNGversion(vstr = "3.5.0")

test_that("Basic test functions in shapley.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  x_train <- tail(Boston[, x_var], 50)

  # Load premade lm model. Path needs to be relative to testthat directory in the package
  model <- readRDS("model_objects/lm_model_object.rds")

  # Prepare the data for explanation
  explainer <- shapr(x_train, model)

  expect_known_value(explainer, file = "test_objects/shapley_explainer_obj.rds")
})


test_that("Testing data input to shapr in shapley.R", {

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
    expect_silent(shapr(xy_train_full_df, l[[i]]))

    # Expect message that feature_labels is ignored
    expect_message(shapr(xy_train_full_df, l[[i]], feature_labels = x_var_sub))
    expect_message(shapr(xy_train_full_df, l[[i]], feature_labels = x_var))

    # Expect error, giving error message that indicates that x misses columns used by the model
    expect_error(shapr(xy_train_missing_lstat_df, l[[i]]))

    # Expect error when x_train don't have column names
    expect_error(shapr(xy_train_full_df_no_colnames, l[[i]], feature_labels = x_var_sub))
  }
})
