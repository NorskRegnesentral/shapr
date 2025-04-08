test_that("semi_deterministic_samplign: not paired sampling", {
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

test_that("semi_deterministic_samplign: not regular sampling", {
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

test_that("semi_deterministic_samplign: not symmetric sampling", {
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
