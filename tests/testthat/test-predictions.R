library(shapr)
library(testthat)

context("test-predictions.R")

test_that("Test prediction", {

  # Example -----------
  data("Boston", package = "MASS")
  dt_train <- data.table::as.data.table(Boston)
  features <- c("lstat", "rm", "dis", "indus")
  n_combinations <- 10
  n_features <- 4
  prediction_zero <- .5
  n_xtest <- 8
  #
  dt_combinations <- feature_combinations(
    m = n_features,
    exact = TRUE,
    n_combinations = n_combinations,
    weight_zero_m = 10^6,
    group_num = NULL
  )
  #
  explainer <- list()
  explainer$model <- stats::lm(formula = "medv ~ lstat + rm + dis + indus",
                               data = head(dt_train, -n_xtest))
  explainer$x_test <- tail(dt_train[, .SD, .SDcols = features], n_xtest)
  explainer$W <- matrix(1, nrow = n_features + 1, ncol = n_combinations)
  explainer$is_groupwise <- FALSE
  explainer$feature_labels <- c("lstat", "rm", "dis", "indus")
  #
  dt <- dt_train[, .SD, .SDcols = features][rep(1:.N, 4)]

  dt[, id := rep_len(1:n_xtest, .N)]
  dt[, id_combination := rep_len(1:n_combinations, .N), id]
  dt[, w := runif(.N)]
  max_id_combination <- dt[, max(id_combination)]
  dt <- dt[!(id_combination == max_id_combination)]
  dt_lastrows <- data.table::data.table(
    explainer$x_test,
    id = 1:n_xtest,
    id_combination = max_id_combination,
    w = 1.0
  )
  dt <- rbind(dt, dt_lastrows, dt_lastrows, dt_lastrows)
  dt <- merge(dt, dt_combinations, on = id_combination)
  x <- prediction(dt, prediction_zero, explainer)

  # Test -----------
  lnms <- c("dt", "model", "p", "x_test")
  expect_equal(class(x), c("shapr", "list"))
  expect_equal(names(x), lnms)
  expect_equal(x$model, explainer$model)
  expect_equal(x$x_test, explainer$x_test)
  expect_equal(x$p, predict_model(explainer$model, explainer$x_test))
  expect_true(data.table::is.data.table(x$dt))
  expect_equal(ncol(x$dt), n_features + 1)
  expect_equal(nrow(x$dt), nrow(explainer$x_test))
  expect_equal(colnames(x$dt), c("none", features))

  # Test errors
  expect_error(prediction(dt[id < n_xtest], prediction_zero, explainer))

  # Example 2 - with groups ----------
  data("Boston", package = "MASS")
  dt_train <- data.table::as.data.table(Boston)
  features <- c("lstat", "rm", "dis", "indus")
  n_combinations <- 10
  n_features <- 4
  n_groups <- 2
  prediction_zero <- .5
  n_xtest <- 8
  #
  dt_combinations <- feature_combinations(
    m = n_features,
    exact = TRUE,
    n_combinations = n_combinations,
    weight_zero_m = 10^6,
    group_num = NULL
  )
  #
  explainer <- list()
  explainer$model <- stats::lm(formula = "medv ~ lstat + rm + dis + indus",
                               data = head(dt_train, -n_xtest))
  explainer$x_test <- tail(dt_train[, .SD, .SDcols = features], n_xtest)

  explainer$group <- list("group1" = c("lstat", "dis"), "group2" = c("rm", "indus"))
  explainer$group_num <- list("group1" = c(1, 3), "group2" = c(2, 4))
  explainer$is_groupwise <- TRUE
  explainer$W <- matrix(1, nrow = n_groups + 1, ncol = n_combinations)
  explainer$feature_labels <- c("lstat", "rm", "dis", "indus")

  dt <- dt_train[, .SD, .SDcols = features][rep(1:.N, 4)]
  dt[, id := rep_len(1:n_xtest, .N)]
  dt[, id_combination := rep_len(1:n_combinations, .N), id]
  dt[, w := runif(.N)]
  max_id_combination <- dt[, max(id_combination)]
  #
  dt <- dt[!(id_combination == max_id_combination)]
  dt_lastrows <- data.table::data.table(
    explainer$x_test,
    id = 1:n_xtest,
    id_combination = max_id_combination,
    w = 1.0
  )
  dt0 <- rbind(dt, dt_lastrows, dt_lastrows, dt_lastrows)
  #
  dt0 <- merge(dt0, dt_combinations, on = id_combination)
  x <- prediction(dt0, prediction_zero, explainer)


  # Test -----------
  lnms <- c("dt", "model", "p", "x_test")
  expect_equal(class(x), c("shapr", "list"))
  expect_equal(names(x), lnms)
  expect_equal(x$model, explainer$model)
  expect_equal(x$x_test, explainer$x_test)

  expect_equal(x$p, predict_model(explainer$model, explainer$x_test))


  expect_true(data.table::is.data.table(x$dt))
  expect_equal(ncol(x$dt), n_groups + 1)
  expect_equal(nrow(x$dt), nrow(explainer$x_test))
  expect_equal(colnames(x$dt), c("none", paste0("group", 1:n_groups)))

  # Test errors
  expect_error(prediction(dt[id < n_xtest], prediction_zero, explainer))

})
