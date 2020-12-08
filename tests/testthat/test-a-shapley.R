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

  expect_known_value(explainer, file = "test_objects/shapley_explainer_obj.rds",
                     update = FALSE)
})


test_that("Testing data input to shapr in shapley.R", {

  # Data -----------
  data("Boston", package = "MASS")
  y_var <- "medv"
  x_train <- tail(Boston, -6)
  y_train <- tail(Boston[, y_var], -6)
  y_train_binary <- as.factor(tail((Boston[, y_var]>20)*1, -6))

  # convert to factors for testing purposes
  x_train$rad <- factor(round(x_train$rad))
  x_train$chas <- factor(round(x_train$chas))

  train_df <- cbind(x_train, y_train,y_train_binary)


  x_var_numeric <- c("lstat", "rm", "dis", "indus")
  x_var_factor <- c("lstat", "rm", "dis", "indus", "rad", "chas")

  train_df_used_numeric <- x_train[,x_var_numeric]
  train_df_used_factor <- x_train[,x_var_factor]

  formula_numeric <- as.formula(paste0("y_train ~ ",paste0(x_var_numeric,collapse="+")))
  formula_factor <- as.formula(paste0("y_train ~ ",paste0(x_var_factor,collapse="+")))

  formula_binary_numeric <- as.formula(paste0("y_train_binary ~ ",paste0(x_var_numeric,collapse="+")))
  formula_binary_factor <- as.formula(paste0("y_train_binary ~ ",paste0(x_var_factor,collapse="+")))

  dummylist <- make_dummies(traindata = x_train[, x_var_factor], testdata = x_train[, x_var_factor])

  # List of models to run silently
  l_numeric <- list(
    stats::lm(formula_numeric, data = train_df),
    stats::glm(formula_numeric, data = train_df),
    mgcv::gam(formula_numeric, data = train_df))

  l_factor <- list(stats::lm(formula_factor, data = train_df),
                   stats::glm(formula_factor, data = train_df),
                   mgcv::gam(formula_factor, data = train_df)
  )

  l_message <- list(xgboost::xgboost(data = dummylist$train_dummies, label = y_train,
                                     nrounds = 3, verbose = FALSE)
  )

  l_message[[1]]$dummylist <- dummylist$obj


  for (i in seq_along(l_numeric)) {
    expect_silent(shapr(train_df_used_numeric,l_numeric[[i]])) # No modification
    expect_message(shapr(train_df,l_numeric[[i]])) # Features dropped
  }

  for (i in seq_along(l_factor)) {
    expect_silent(shapr(train_df_used_factor,l_factor[[i]])) # No modification
    expect_message(shapr(train_df,l_factor[[i]])) # Features dropped
  }

  for (i in seq_along(l_message)) {
    expect_message(shapr(train_df_used_factor,l_message[[i]])) # Factor reordering
    expect_message(shapr(train_df,l_message[[i]])) # Factor reordering + features dropped
  }

  # Add tests here for differnet types "errors" from test-models + the original ones below


  ##########OLD ##########

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
