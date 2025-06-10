skip_on_cran()

test_that("output_sage_independence_lm", {
  set.seed(123)

  expect_snapshot_rds(
    {
      y_train_numeric <- data_train[[y_var_numeric]]

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        seed = 1,
        iterative = FALSE,
        sage = TRUE,
        response = y_train_numeric
      )
    },
    "output_sage_independence_lm"
  )
})

test_that("output_sage_gaussian_xgboost", {
  set.seed(123)

  expect_snapshot_rds(
    {
      library(xgboost)

      x_numeric <- data_complete[, ..x_var_numeric]
      y_numeric <- data_complete[, get(y_var_numeric)]

      model_xgboost <- xgboost(
        data = as.matrix(x_numeric),
        label = y_numeric,
        nround = 20,
        verbose = FALSE
      )

      p0_full <- mean(y_numeric)

      explain(
        testing = TRUE,
        model = model_xgboost,
        x_explain = x_numeric,
        x_train = x_numeric,
        approach = "gaussian",
        phi0 = p0_full,
        seed = 1,
        iterative = FALSE,
        sage = TRUE,
        response = y_numeric
      )
    },
    "output_sage_gaussian_xgboost"
  )
})

test_that("output_sage_empirical_lm_iter", {
  set.seed(123)

  expect_snapshot_rds(
    {
      y_train_numeric <- data_train[[y_var_numeric]]

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "empirical",
        phi0 = p0,
        seed = 1,
        iterative = TRUE,
        sage = TRUE,
        response = y_train_numeric
      )
    },
    "output_sage_empirical_lm_iter"
  )
})

test_that("output_sage_copula_lm", {
  set.seed(123)

  expect_snapshot_rds(
    {
      y_train_numeric <- data_train[[y_var_numeric]]

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        iterative = FALSE,
        sage = TRUE,
        response = y_train_numeric
      )
    },
    "output_sage_copula_lm"
  )
})
