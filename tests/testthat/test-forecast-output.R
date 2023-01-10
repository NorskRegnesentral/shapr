
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
      "multiple_output_ar_numeric"
    )
})


