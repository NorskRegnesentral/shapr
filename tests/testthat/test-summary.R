skip_on_cran()


test_that("summary_explain", {
  set.seed(123)

  obj_explain <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "independence",
    phi0 = p0,
    seed = 1
  )

  expect_snapshot_rds(
    summary(obj_explain),
    "summary_explain"
  )
})

test_that("summary_explain_forecast", {
  skip_if_not_installed("forecast")

  set.seed(123)

  obj_explain_forecast <- explain_forecast(
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
  )

  expect_snapshot_rds(
    summary(obj_explain_forecast),
    "summary_explain_forecast"
  )
})
