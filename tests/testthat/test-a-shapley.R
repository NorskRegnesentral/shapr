library(testthat)
library(shapr)
library(xgboost)

context("test-shapley.R")

test_that("Test functions in shapley.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  x_train <- as.matrix(tail(Boston[, x_var], 50))
  y_train <- tail(Boston[, y_var], 50)
  x_test <- as.matrix(head(Boston[, x_var], 2))

  #setwd("./tests/testthat") # Uncomment when running manually

  # Load premade xgboost model. Path needs to be relative to testthat directory in the package
  model <- readRDS("test_objects/xgboost_model_object.rds")

  # Prepare the data for explanation
  explainer <- shapr(x_train, model)

  expect_known_value(explainer,file = "test_objects/shapley_explainer_obj.rds")

})
