skip_on_cran()

test_that("output_semi_determ_iterative_reach_exact", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      seed = 1,
      iterative = TRUE,
      verbose = c("basic", "convergence", "shapley"),
      extra_computation_args = list(semi_deterministic_sampling = TRUE)
    ),
    "output_semi_determ_iterative_reach_exact"
  )
})


test_that("output_semi_determ_group_converges_tol", {
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
      phi0 = p0,
      seed = 1,
      max_n_coalitions = 20,
      iterative_args = list(
        initial_n_coalitions = 6,
        convergence_tol = 0.00001
      ),
      iterative = TRUE,
      verbose = c("basic", "convergence", "shapley"),
      extra_computation_args = list(semi_deterministic_sampling = TRUE)
    ),
    "output_semi_determ_group_converges_tol"
  )
})

test_that("output_semi_determ_ts_timeseries", {
  expect_snapshot_rds(
    explanation_timeseries <- explain(
      testing = TRUE,
      model = model_lm_ts,
      x_explain = x_explain_ts,
      x_train = x_train_ts,
      approach = "timeseries",
      phi0 = p0_ts,
      seed = 1,
      group = group_ts,
      iterative = TRUE,
      iterative_args = list(initial_n_coalitions = 6),
      max_n_coalitions = 12,
      verbose = c("basic", "convergence", "shapley"),
      extra_computation_args = list(semi_deterministic_sampling = TRUE)
    ),
    "output_semi_determ_lm_timeseries_method"
  )
})
