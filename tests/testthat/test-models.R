library(shapr)

context("test-models.R")

test_that("Test predict_model", {

  # Data -----------
  data("Boston")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"
  x_train <- as.matrix(tail(Boston[, x_var], -6))
  y_train <- tail(Boston[, y_var], -6)
  x_test <- as.matrix(head(Boston[, x_var], 6))

  # Example 1 (error) -----------
  mod_error <- 1
  expect_error(predict_model(mod_error, x_test))

  # Example 2 (lm) -----------

  # Example 3 (glm) -----------
  mod_error <- 1

  # Example 4 (ranger) -----------
  mod_error <- 1

  # Example 5 (xgboost) -----------
  mod_error <- 1

  # Example 6 (mgcv) -----------
  mod_error <- 1

})
