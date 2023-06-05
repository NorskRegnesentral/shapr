test_that("forecast_output_ar_numeric", {
  expect_snapshot_rds(
    explain_forecast(
      model = model_ar_temp,
      y = data[, "Temp"],
      train_idx = 2:151,
      explain_idx = 152:153,
      explain_y_lags = 2,
      horizon = 3,
      approach = "empirical",
      prediction_zero = p0_ar,
      group_lags = FALSE,
      n_batches = 1,
      timing = FALSE
    ),
    "forecast_output_ar_numeric"
  )
})

test_that("forecast_output_arima_numeric", {
  expect_snapshot_rds(
    explain_forecast(
      model = model_arima_temp,
      y = data[1:150, "Temp"],
      xreg = data[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      explain_xreg_lags = 2,
      horizon = 3,
      approach = "empirical",
      prediction_zero = p0_ar,
      group_lags = FALSE,
      n_batches = 1,
      timing = FALSE
    ),
    "forecast_output_arima_numeric"
  )
})

test_that("forecast_output_arima_numeric_no_xreg", {
  expect_snapshot_rds(
    explain_forecast(
      model = model_arima_temp_noxreg,
      y = data[1:150, "Temp"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      horizon = 3,
      approach = "empirical",
      prediction_zero = p0_ar,
      group_lags = FALSE,
      n_batches = 1,
      timing = FALSE
    ),
    "forecast_output_arima_numeric_no_xreg"
  )
})

test_that("forecast_output_forecast_ARIMA_group_numeric", {
  expect_snapshot_rds(
    explain_forecast(
      model = model_forecast_ARIMA_temp,
      y = data[1:150, "Temp"],
      xreg = data[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      explain_xreg_lags = 2,
      horizon = 3,
      approach = "empirical",
      prediction_zero = p0_ar,
      group_lags = TRUE,
      n_batches = 1,
      timing = FALSE
    ),
    "forecast_output_forecast_ARIMA_group_numeric"
  )
})


test_that("ARIMA gives the same output with different horizons", {
  h3 <- explain_forecast(
    model = model_arima_temp,
    y = data[1:150, "Temp"],
    xreg = data[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 3,
    approach = "empirical",
    prediction_zero = p0_ar[1:3],
    group_lags = FALSE,
    n_batches = 1,
    timing = FALSE, n_combinations = 50
  )


  h2 <- explain_forecast(
    model = model_arima_temp,
    y = data[1:150, "Temp"],
    xreg = data[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 2,
    approach = "empirical",
    prediction_zero = p0_ar[1:2],
    group_lags = FALSE,
    n_batches = 1,
    timing = FALSE, n_combinations = 50
  )

  h1 <- explain_forecast(
    model = model_arima_temp,
    y = data[1:150, "Temp"],
    xreg = data[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 1,
    approach = "empirical",
    prediction_zero = p0_ar[1],
    group_lags = FALSE,
    n_batches = 1,
    timing = FALSE, n_combinations = 50
  )

  cols_horizon1 <- h2$internal$objects$cols_per_horizon[[1]]
  expect_equal(
    h2$shapley_values[horizon == 1, ..cols_horizon1],
    h1$shapley_values[horizon == 1, ..cols_horizon1]
  )

  expect_equal(
    h3$shapley_values[horizon == 1, ..cols_horizon1],
    h1$shapley_values[horizon == 1, ..cols_horizon1]
  )

  cols_horizon2 <- h2$internal$objects$cols_per_horizon[[2]]
  expect_equal(
    h3$shapley_values[horizon == 2, ..cols_horizon2],
    h2$shapley_values[horizon == 2, ..cols_horizon2]
  )
})

test_that("ARIMA gives the same output with different horizons with grouping", {
  h3 <- explain_forecast(
    model = model_arima_temp,
    y = data[1:150, "Temp"],
    xreg = data[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 3,
    approach = "empirical",
    prediction_zero = p0_ar[1:3],
    group_lags = TRUE,
    n_batches = 1,
    timing = FALSE, n_combinations = 50
  )


  h2 <- explain_forecast(
    model = model_arima_temp,
    y = data[1:150, "Temp"],
    xreg = data[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 2,
    approach = "empirical",
    prediction_zero = p0_ar[1:2],
    group_lags = TRUE,
    n_batches = 1,
    timing = FALSE, n_combinations = 50
  )

  h1 <- explain_forecast(
    model = model_arima_temp,
    y = data[1:150, "Temp"],
    xreg = data[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 1,
    approach = "empirical",
    prediction_zero = p0_ar[1],
    group_lags = TRUE,
    n_batches = 1,
    timing = FALSE, n_combinations = 50
  )

  expect_equal(
    h2$shapley_values[horizon == 1],
    h1$shapley_values[horizon == 1]
  )

  expect_equal(
    h3$shapley_values[horizon == 1],
    h1$shapley_values[horizon == 1]
  )

  expect_equal(
    h3$shapley_values[horizon == 2],
    h2$shapley_values[horizon == 2]
  )
})

test_that("forecast_output_arima_numeric_no_lags", {
  # TODO: Need to check out this output. It gives lots of warnings, which indicates something might be wrong.
  expect_snapshot_rds(
    explain_forecast(
      model = model_arima_temp,
      y = data[1:150, "Temp"],
      xreg = data[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 0,
      explain_xreg_lags = 0,
      horizon = 3,
      approach = "independence",
      prediction_zero = p0_ar,
      group_lags = FALSE,
      n_batches = 1,
      timing = FALSE
    ),
    "forecast_output_arima_numeric_no_lags"
  )
})
