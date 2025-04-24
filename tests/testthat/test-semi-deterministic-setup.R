test_that("semi_deterministic_sampling: not paired sampling", {
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        seed = 1,
        extra_computation_args = list(
          paired_shap_sampling = FALSE,
          semi_deterministic_sampling = TRUE
        )
      )
    },
    error = TRUE
  )
})

test_that("semi_deterministic_sampling: not regular sampling", {
  expect_snapshot(
    {
      explain_forecast(
        testing = TRUE,
        model = model_ar_temp,
        y = data_arima[, "Temp"],
        train_idx = 2:151,
        explain_idx = 152:153,
        explain_y_lags = 2,
        horizon = 3,
        approach = "empirical",
        phi0 = p0_ar,
        seed = 1,
        group_lags = FALSE,
        extra_computation_args = list(
          paired_shap_sampling = TRUE,
          semi_deterministic_sampling = TRUE
        )
      )
    },
    error = TRUE
  )
})

test_that("semi_deterministic_sampling: not symmetric sampling", {
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(1:2, 3, 4:5),
        confounding = NULL,
        extra_computation_args = list(
          paired_shap_sampling = TRUE,
          semi_deterministic_sampling = TRUE
        )
      )
    },
    error = TRUE
  )
})

test_that("setup_semi_determ_n_determ_sample_coal_18", {
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
  expect_snapshot_rds(ex_18, "setup_semi_determ_n_determ_sample_coal_18")
})

test_that("setup_semi_determ_n_determ_sample_coal_20", {
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
  expect_snapshot_rds(ex_20, "setup_semi_determ_n_determ_sample_coal_20")
})
