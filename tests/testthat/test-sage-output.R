skip_on_cran()

test_that("output_sage_independence_lm", {
  set.seed(123)

  expect_snapshot_rds(
    {
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
  skip_if_not_installed("xgboost")

  set.seed(123)

  expect_snapshot_rds(
    {
      model_xgboost <- xgboost::xgboost(
        data = as.matrix(x_train_numeric),
        label = y_train_numeric,
        nround = 20,
        verbose = FALSE
      )

      explain(
        testing = TRUE,
        model = model_xgboost,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        iterative = FALSE,
        sage = TRUE,
        response = y_train_numeric
      )
    },
    "output_sage_gaussian_xgboost"
  )
})

test_that("output_sage_empirical_lm", {
  set.seed(123)

  expect_snapshot_rds(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "empirical",
        phi0 = p0,
        seed = 1,
        iterative = FALSE,
        sage = TRUE,
        response = y_train_numeric,
        max_n_coalitions = 10
      )
    },
    "output_sage_empirical_lm"
  )
})

test_that("output_sage_copula_lm_iter", {
  set.seed(123)

  expect_snapshot_rds(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        iterative = TRUE,
        sage = TRUE,
        response = y_train_numeric
      )
    },
    "output_sage_copula_lm_iter"
  )
})

test_that("output_sage_groups_lm_copula", {
  set.seed(123)

  expect_snapshot_rds(
    {
      groups <- list(A = c("Temp", "Month"), B = c("Wind", "Solar.R"))

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_train_numeric,
        groups = groups
      )
    },
    "output_sage_groups_lm_copula"
  )
})

test_that("output_sage_loss_mae", {
  set.seed(123)

  expect_snapshot_rds(
    {
      loss_mae <- function(y, y_hat) {
        colMeans(abs(y - y_hat))
      }

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_train_numeric,
        loss_func = loss_mae
      )
    },
    "output_sage_loss_mae"
  )
})

test_that("output_sage_loss_mape", {
  set.seed(123)

  expect_snapshot_rds(
    {
      loss_mape <- function(y, y_hat) {
        eps <- 1e-8
        denom <- pmax(abs(y), eps)
        colMeans(abs((y - y_hat) / denom)) * 100
      }

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_train_numeric,
        loss_func = loss_mape
      )
    },
    "output_sage_groups_loss_mape"
  )
})

test_that("output_sage_binary", {
  skip_if_not_installed("xgboost")

  set.seed(123)

  expect_snapshot_rds(
    {
      model_xgboost_binary <- xgboost::xgboost(
        data = as.matrix(x_train_numeric),
        label = y_train_binary,
        nround = 20,
        verbose = FALSE
      )

      explain(
        testing = TRUE,
        model = model_xgboost_binary,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0_binary,
        seed = 1,
        sage = TRUE,
        response = y_train_numeric
      )
    },
    "output_sage_xgboost_binary"
  )
})
