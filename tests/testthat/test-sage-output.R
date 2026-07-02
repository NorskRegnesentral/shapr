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
        scope = "global",
        y_explain = y_train_numeric
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
        scope = "global",
        y_explain = y_train_numeric
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
        scope = "global",
        y_explain = y_train_numeric,
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
        scope = "global",
        y_explain = y_train_numeric
      )
    },
    "output_sage_copula_lm_iter"
  )
})

test_that("output_sage_groups_lm_copula", {
  set.seed(123)

  expect_snapshot_rds(
    {
      groups <- list(A = c("Temp", "Month", "Day"), B = c("Wind", "Solar.R"))

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_train_numeric,
        group = groups
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
        mean(abs(y - y_hat))
      }

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_train_numeric,
        extra_computation_args = list(global_loss_func = loss_mae)
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
        mean(abs((y - y_hat) / denom)) * 100
      }

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "copula",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_train_numeric,
        extra_computation_args = list(global_loss_func = loss_mape)
      )
    },
    "output_sage_loss_mape"
  )
})

test_that("output_sage_xgboost_binary", {
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
        scope = "global",
        y_explain = y_train_binary
      )
    },
    "output_sage_xgboost_binary"
  )
})

test_that("get_results accessors expose shap and sage values symmetrically", {
  set.seed(123)

  explanation_global <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_train_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    seed = 1,
    iterative = FALSE,
    scope = "global",
    y_explain = y_train_numeric
  )

  set.seed(123)

  explanation_local <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_train_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    seed = 1,
    iterative = FALSE,
    scope = "local"
  )

  # Local scope: shap_values_est equals shapley_est, sage_values_est is NULL
  expect_identical(
    get_results(explanation_local, "shap_values_est"),
    get_results(explanation_local, "shapley_est")
  )
  expect_null(get_results(explanation_local, "sage_values_est"))

  # Global scope: sage_values_est equals shapley_est, shap_values_est holds per-observation values
  expect_identical(
    get_results(explanation_global, "sage_values_est"),
    get_results(explanation_global, "shapley_est")
  )
  expect_s3_class(get_results(explanation_global, "shap_values_est"), "data.table")
  expect_equal(
    nrow(get_results(explanation_global, "shap_values_est")),
    nrow(x_train_numeric)
  )

  # `scope` is part of the default output and reflects the requested scope
  expect_identical(get_results(explanation_local, "scope"), "local")
  expect_identical(get_results(explanation_global, "scope"), "global")
  expect_true("scope" %in% names(get_results(explanation_local)))
  expect_true("scope" %in% names(get_results(explanation_global)))

  # The default output includes `shap_values_est` only for global (SAGE) explanations; `sage_values_est` is
  # never part of the default (it duplicates `shapley_est` for global and is `NULL` for local)
  expect_false("shap_values_est" %in% names(get_results(explanation_local)))
  expect_false("sage_values_est" %in% names(get_results(explanation_local)))
  expect_true("shap_values_est" %in% names(get_results(explanation_global)))
  expect_false("sage_values_est" %in% names(get_results(explanation_global)))
})
