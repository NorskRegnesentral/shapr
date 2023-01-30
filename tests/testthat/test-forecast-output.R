
test_that("forecast_output_ar_numeric", {
    expect_snapshot_rds(
      explain_forecast(model = model_ar_temp,
                 data = data[, "Temp"],
                 train_idx = 2:151,
                 explain_idx = 152:153,
                 lags = list(data=2),
                 horizon = 3,
                 approach = "empirical",
                 prediction_zero = p0_ar,
                 group_lags = FALSE
      ),
      "forecast_output_ar_numeric"
    )
})

test_that("forecast_output_arima_numeric", {
    expect_snapshot_rds(
      explain_forecast(model = model_arima_temp,
                 data = data[1:150, "Temp"],
                 reg = data[, "Wind"],
                 train_idx = 2:148,
                 explain_idx = 149:150,
                 lags = list(data=2, reg=2),
                 horizon = 3,
                 approach = "empirical",
                 prediction_zero = p0_ar,
                 group_lags = FALSE
      ),
      "forecast_output_arima_numeric"
    )
})

test_that("forecast_output_forecast_ARIMA_group_numeric", {
  expect_snapshot_rds(
    explain_forecast(model = model_forecast_ARIMA_temp,
                     data = data[1:150, "Temp"],
                     reg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     lags = list(data=2, reg=2),
                     horizon = 3,
                     approach = "empirical",
                     prediction_zero = p0_ar,
                     group_lags = TRUE
    ),
    "forecast_output_forecast_ARIMA_group_numeric"
  )
})

