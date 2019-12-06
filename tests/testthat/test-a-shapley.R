library(testthat)
library(shapr)

context("test-shapley.R")

if (as.numeric(version$minor) >= 6.0) RNGkind(sample.kind = "Rounding")

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
    newdata[,1] # Always giving the first argument of newdata as the prediction
  }

  #### Running tests ####

  # Expect silent
  expect_silent(shapr(x_train, model1))
  expect_silent(shapr(xy_train_full_df, model2))
  expect_silent(shapr(xy_train_full_df, model3))
  expect_silent(shapr(xy_train_full_df, model4,feature_labels = x_var))

  # Expect message that feature_labels is ignored
  expect_message(shapr(xy_train_full_df, model1, feature_labels = x_var_sub))
  expect_message(shapr(xy_train_full_df, model1, feature_labels = x_var))
  expect_message(shapr(xy_train_full_df, model2, feature_labels = x_var_sub))
  expect_message(shapr(xy_train_full_df, model3, feature_labels = x_var_sub))

  # Expect error, giving error message that throws indicates that the x misses columns used by the model
  expect_error(shapr(xy_train_missing_lstat_df, model1)) # Should give better error message
  expect_error(shapr(xy_train_missing_lstat_df, model2)) # Should give better error message

  # Expect error that feature_labels is not in training data or used by the model
  expect_error(shapr(xy_train_full_df, model4,feature_labels = not_x_var)) # Should throw an error, does not do that
  expect_error(shapr(xy_train_full_df, model4,feature_labels = not_even_var)) # Should give better error message

  # Expect error, that feature_labels is missing
  expect_error(shapr(xy_train_full_df, model4))

})
