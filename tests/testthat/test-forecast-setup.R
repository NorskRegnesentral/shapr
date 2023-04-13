



test_that("error with custom model without providing predict_model",{
  set.seed(123)


  expect_snapshot({
    # Custom model with no predict_model
    model_custom_arima_temp <- model_arima_temp
    class(model_custom_arima_temp) <- "whatever"

    explain_forecast(model = model_custom_arima_temp,
                     y = data[1:150, "Temp"],
                     xreg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = 2,
                     explain_xreg_lags = 2,
                     horizon = 3,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1)
    },
    error = T
  )

})


test_that("erroneous input: `x_train/x_explain`", {
  set.seed(123)

  expect_snapshot({
    # not vector or one-column data.table/matrix
    y_wrong_format <- data[, c("Temp","Wind")]# TODO: Change error message

    explain_forecast(model = model_arima_temp,
                     y = y_wrong_format,
                     xreg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = 2,
                     explain_xreg_lags = 2,
                     horizon = 3,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1)
  },
  error = T)

  expect_snapshot({
    # not correct dimension
    xreg_wrong_format <- data[, c("Temp","Wind")] # TODO: Change error message

    explain_forecast(model = model_arima_temp,
                     y = data[1:150, "Temp"],
                     xreg = xreg_wrong_format,
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = 2,
                     explain_xreg_lags = 2,
                     horizon = 3,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1)
  },
  error = T)

  expect_snapshot({
    # missing column names x_train
    xreg_no_column_names <- data[, "Wind"] # TODO: Change error message
    names(xreg_no_column_names) <- NULL

    explain_forecast(model = model_arima_temp,
                     y = data[1:150, "Temp"],
                     xreg = xreg_no_column_names,
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = 2,
                     explain_xreg_lags = 2,
                     horizon = 3,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1)
  },
  error = T)

})

test_that("erroneous input: `model`", {
  set.seed(123)

  expect_snapshot({
    # no model passed
    explain_forecast(y = data[1:150, "Temp"],
                     xreg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = 2,
                     explain_xreg_lags = 2,
                     horizon = 3,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1)
  },
  error = T)



})


test_that("erroneous input: `prediction_zero`", {
  set.seed(123)

  expect_snapshot({
    # incorrect length
    p0_wrong_length <- p0_ar[1:2]

    explain_forecast(model = model_arima_temp,
                     y = data[1:150, "Temp"],
                     xreg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = 2,
                     explain_xreg_lags = 2,
                     horizon = 3,
                     approach = "independence",
                     prediction_zero = p0_wrong_length,
                     n_batches = 1)
  },
    error = T)

})

test_that("erroneous input: `n_combinations`", {
  set.seed(123)

  expect_snapshot({
    # Too low n_combinations (smaller than # features)
    horizon = 3
    explain_y_lags = 2
    explain_xreg_lags = 2

    n_combinations = horizon+explain_y_lags+explain_xreg_lags-1 # TODO: Change error message to be more informative

    explain_forecast(model = model_arima_temp,
                     y = data[1:150, "Temp"],
                     xreg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = explain_y_lags,
                     explain_xreg_lags = explain_xreg_lags,
                     horizon = horizon,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1,
                     n_combinations = n_combinations,
                     group_lags = FALSE)
  },
  error = T)


  expect_snapshot({
    # Too low n_combinations (smaller than # groups)
    horizon = 3
    explain_y_lags = 2
    explain_xreg_lags = 2

    n_combinations = 1+1 # TODO: Change error message to be more informative

    explain_forecast(model = model_arima_temp,
                     y = data[1:150, "Temp"],
                     xreg = data[, "Wind"],
                     train_idx = 2:148,
                     explain_idx = 149:150,
                     explain_y_lags = explain_y_lags,
                     explain_xreg_lags = explain_xreg_lags,
                     horizon = horizon,
                     approach = "independence",
                     prediction_zero = p0_ar,
                     n_batches = 1,
                     n_combinations = n_combinations,
                     group_lags = TRUE)
  },
  error = T)

})


#### TODO: Add checks for the following input parameters here:

# train_idx (incorrect type, incompatible indexes)
# explain_idx (incorrect type, incompatible indexes)
# explain_y_lags (negative number, non-integer value) Should we allow zero? YES
# explain_y_lags (negative number, non-integer value) Should we allow zero? YES
# horizon (negative number, non-integer value) Should we allow zero? YES

