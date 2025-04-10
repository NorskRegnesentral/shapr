test_that("output_semi_determ_sampling_n_determ_sample_coal_18", {
  # Should only deterministically include empty and grand coalition
  ex_18 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    seed = 1,
    max_n_coalitions = 18,
    n_MC_samples = 50, # Just to speed up the computations
    iterative = FALSE,
    verbose = c("basic", "convergence", "shapley"),
    extra_computation_args = list(semi_deterministic_sampling = TRUE)
  )

  # Check that the number of deterministic and sampled coalitions is correct
  expect_equal(ex_18$internal$objects$X[, sum(is.na(sample_freq))], 2)
  expect_equal(ex_18$internal$objects$X[, sum(!is.na(sample_freq))], 18 - 2)

  # Check that the coalition sizes are correct
  ex_18_expected <- data.frame(Var1 = factor(c(0, 5)), Freq = c(1, 1))
  expect_equal(ex_18_expected, as.data.frame(table(ex_18$internal$objects$X[is.na(sample_freq), coalition_size])))

  # Check the rds objects
  expect_snapshot_rds(ex_18, "output_semi_determ_sampling_n_determ_sample_coal_18")
})

test_that("output_semi_determ_sampling_n_determ_sample_coal_20", {
  # Should deterministically include empty and grand coalition, and coalitions of size 1 and 4 (m - 1)
  ex_20 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    seed = 1,
    max_n_coalitions = 20,
    n_MC_samples = 50, # Just to speed up the computations
    iterative = FALSE,
    verbose = c("basic", "convergence", "shapley"),
    extra_computation_args = list(semi_deterministic_sampling = TRUE)
  )

  # Check that the number of deterministic and sampled coalitions is correct
  expect_equal(ex_20$internal$objects$X[, sum(is.na(sample_freq))], 12)
  expect_equal(ex_20$internal$objects$X[, sum(!is.na(sample_freq))], 20 - 12)

  # Check that the coalition sizes are correct
  ex_20_expected <- data.frame(Var1 = factor(c(0, 1, 4, 5)), Freq = c(1, 5, 5, 1))
  expect_equal(ex_20_expected, as.data.frame(table(ex_20$internal$objects$X[is.na(sample_freq), coalition_size])))

  # Check the rds objects
  expect_snapshot_rds(ex_20, "output_semi_determ_sampling_n_determ_sample_coal_20")
})

test_that("output_semi_determ_sampling_iterative_reach_exact", {
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
    "output_semi_determ_sampling_iterative_reach_exact"
  )
})


test_that("output_semi_determ_sampling_group_converges_tol", {
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
    "output_semi_determ_sampling_group_converges_tol"
  )
})

test_that("output_semi_determ_sampling_ts_timeseries", {
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
    "output_semi_determ_sampling_lm_timeseries_method"
  )
})
