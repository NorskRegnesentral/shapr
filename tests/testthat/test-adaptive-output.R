# lm_numeric with different approaches

test_that("output_lm_numeric_independence_reach_exact", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_combinations=10,
      timing = FALSE,
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
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_combinations=10,
      adaptive_arguments = list(initial_n_combinations = 10,
                                convergence_tolerance = 0.1),
      timing = FALSE,
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
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_combinations=10,
      adaptive_arguments = list(initial_n_combinations = 10,
                                convergence_tolerance = 0.001,
                                reduction_factor_vec = rep(10^(-5),10),
                                max_iter = 8),
      timing = FALSE,
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_independence_converges_maxit"
  )
})


