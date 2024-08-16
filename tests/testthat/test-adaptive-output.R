# lm_numeric with different approaches

test_that("output_lm_numeric_independence_reach_exact", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_independence_reach_exact"
  )
})

test_that("output_lm_numeric_independence_converges_tol", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.1
      ),
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_independence_converges_tol"
  )
})

test_that("output_lm_numeric_independence_converges_maxit", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.001,
        reduction_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_independence_converges_maxit"
  )
})

test_that("output_lm_numeric_independence_converges_max_n_coalitions", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      max_n_coalitions = 20,
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_independence_converges_max_n_coalitions"
  )
})


test_that("output_lm_numeric_gaussian_group_converges_tol", {
  groups <- list(
    A = c("Solar.R", "Wind"),
    B = c("Temp", "Month"),
    C = "Day"
  )

  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      group = groups,
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 5,
        convergence_tolerance = 0.1
      ),
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_gaussian_group_converges_tol"
  )
})
