skip_if_not_installed("forecast")
skip_on_cran()

test_that("forecast_output_ar_numeric", {
  expect_snapshot_rds(
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
      group_lags = FALSE
    ),
    "forecast_output_ar_numeric"
  )
})

test_that("forecast_output_arima_numeric", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_arima_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      explain_xreg_lags = 2,
      horizon = 3,
      approach = "empirical",
      phi0 = p0_ar,
      seed = 1,
      group_lags = FALSE,
      max_n_coalitions = 150,
      iterative = FALSE
    ),
    "forecast_output_arima_numeric"
  )
})

test_that("forecast_output_arima_numeric_iterative", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_arima_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 3:148,
      explain_idx = 149:150,
      explain_y_lags = 3,
      explain_xreg_lags = 3,
      horizon = 3,
      approach = "empirical",
      phi0 = p0_ar,
      seed = 1,
      group_lags = FALSE,
      max_n_coalitions = 150,
      iterative = TRUE,
      iterative_args = list(initial_n_coalitions = 10)
    ),
    "forecast_output_arima_numeric_iterative"
  )
})

test_that("forecast_output_arima_numeric_iterative_groups", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_arima_temp2,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, c("Wind", "Solar.R", "Ozone")],
      train_idx = 3:148,
      explain_idx = 149:150,
      explain_y_lags = 3,
      explain_xreg_lags = c(3, 3, 3),
      horizon = 3,
      approach = "empirical",
      phi0 = p0_ar,
      seed = 1,
      group_lags = TRUE,
      max_n_coalitions = 150,
      iterative = TRUE,
      iterative_args = list(initial_n_coalitions = 10, convergence_tol = 7e-3)
    ),
    "forecast_output_arima_numeric_iterative_groups"
  )
})

test_that("forecast_output_arima_numeric_no_xreg", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_arima_temp_noxreg,
      y = data_arima[1:150, "Temp"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      horizon = 3,
      approach = "empirical",
      phi0 = p0_ar,
      seed = 1,
      group_lags = FALSE
    ),
    "forecast_output_arima_numeric_no_xreg"
  )
})

# Old snap does not correspond to the results from the master branch, why is unclear.
test_that("forecast_output_forecast_ARIMA_group_numeric", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_forecast_ARIMA_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      explain_xreg_lags = 2,
      horizon = 3,
      approach = "empirical",
      phi0 = p0_ar,
      seed = 1,
      group_lags = TRUE
    ),
    "forecast_output_forecast_ARIMA_group_numeric"
  )
})

test_that("forecast_output_arima_numeric_no_lags", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_arima_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 0,
      explain_xreg_lags = 0,
      horizon = 3,
      approach = "independence",
      phi0 = p0_ar,
      seed = 1,
      group_lags = FALSE
    ),
    "forecast_output_arima_numeric_no_lags"
  )
})

test_that("forecast_output_forecast_ARIMA_manual_group_numeric", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_forecast_ARIMA_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      explain_xreg_lags = 2,
      horizon = 2,
      approach = "empirical",
      phi0 = p0_ar[1:2],
      seed = 1,
      group_lags = FALSE,
      group = list(
        Temp = c("Temp.1", "Temp.2"),
        Wind = c("Wind.1", "Wind.2", "Wind.F1", "Wind.F2")
      )
    ),
    "forecast_output_forecast_ARIMA_manual_group_numeric"
  )
})

test_that("forecast_output_forecast_ARIMA_manual_group_numeric2", {
  expect_snapshot_rds(
    explain_forecast(
      testing = TRUE,
      model = model_forecast_ARIMA_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = 2,
      explain_xreg_lags = 2,
      horizon = 2,
      approach = "empirical",
      phi0 = p0_ar[1:2],
      seed = 1,
      group_lags = FALSE,
      group = list(
        Group1 = c("Wind.1", "Temp.1", "Wind.F2"),
        Group2 = c("Wind.2", "Temp.2", "Wind.F1")
      )
    ),
    "forecast_output_forecast_ARIMA_manual_group_numeric2"
  )
})
