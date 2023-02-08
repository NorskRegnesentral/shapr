
test_that("forecast_output_ar_numeric", {
    expect_snapshot_rds(
      explain_forecast(model = model_ar_temp,
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
      explain_forecast(model = model_arima_temp,
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

test_that("forecast_output_forecast_ARIMA_group_numeric", {
  expect_snapshot_rds(
    explain_forecast(model = model_forecast_ARIMA_temp,
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


test_that("ARIMA gives the same output for h = 1 when total horizon = 1 and 2", {
  set.seed(123)
  h3 <- explain_forecast(model = model_arima_temp,
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
                         timing = FALSE
  )


  set.seed(123)
  h2 <- explain_forecast(model = model_arima_temp,
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
    timing = FALSE
    )

  set.seed(123)
  h1 <- explain_forecast(model = model_arima_temp,
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
    timing = FALSE)

  expect_equal(h2$shapley_values[1:2, -7], h1$shapley_values)

})
