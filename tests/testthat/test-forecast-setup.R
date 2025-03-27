test_that("error with custom model without providing predict_model", {
  set.seed(123)


  expect_snapshot(
    {
      # Custom model with no predict_model
      model_custom_arima_temp <- model_arima_temp
      class(model_custom_arima_temp) <- "whatever"

      explain_forecast(
        testing = TRUE,
        model = model_custom_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})


test_that("erroneous input: `x_train/x_explain`", {
  set.seed(123)

  expect_snapshot(
    {
      # not vector or one-column data.table/matrix
      y_wrong_format <- data_arima[, c("Temp", "Wind")]

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = y_wrong_format,
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # not correct dimension
      xreg_wrong_format <- data_arima[, c("Temp", "Wind")]

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = xreg_wrong_format,
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # missing column names x_train
      xreg_no_column_names <- data_arima[, "Wind"]
      names(xreg_no_column_names) <- NULL

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = xreg_no_column_names,
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `model`", {
  # R versions earlier than 4.3 gives assigns the error to the internal function instead of the explain_forecast,
  # and therefore marks this as an error (which it is not)
  Rversion_number <- as.numeric(paste0(R.version$major, R.version$minor))
  skip_if_not(Rversion_number >= 43)
  set.seed(123)

  expect_snapshot(
    {
      # no model passed
      explain_forecast(
        testing = TRUE,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})


test_that("erroneous input: `phi0`", {
  set.seed(123)

  expect_snapshot(
    {
      # incorrect length
      p0_wrong_length <- p0_ar[1:2]

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
        approach = "independence",
        phi0 = p0_wrong_length,
        seed = 1
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `max_n_coalitions`", {
  set.seed(123)

  expect_snapshot(
    {
      # Too low max_n_coalitions (smaller than # features)
      horizon <- 3
      explain_y_lags <- 2
      explain_xreg_lags <- 2

      n_coalitions <- horizon + explain_y_lags + explain_xreg_lags - 1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = explain_y_lags,
        explain_xreg_lags = explain_xreg_lags,
        horizon = horizon,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1,
        max_n_coalitions = n_coalitions,
        group_lags = FALSE,
        iterative_args = list("initial_n_coalitions" = 20)
      )
    },
    error = TRUE
  )


  expect_snapshot({
    # Too low n_coalitions (smaller than # groups)
    horizon <- 3
    explain_y_lags <- 2
    explain_xreg_lags <- 2

    n_coalitions <- 1 + 1

    explain_forecast(
      testing = TRUE,
      model = model_arima_temp,
      y = data_arima[1:150, "Temp"],
      xreg = data_arima[, "Wind"],
      train_idx = 2:148,
      explain_idx = 149:150,
      explain_y_lags = explain_y_lags,
      explain_xreg_lags = explain_xreg_lags,
      horizon = horizon,
      approach = "independence",
      phi0 = p0_ar,
      seed = 1,
      max_n_coalitions = n_coalitions,
      group_lags = TRUE
    )
  })
})


test_that("erroneous input: `train_idx`", {
  set.seed(123)

  expect_snapshot(
    {
      # train_idx too short length
      train_idx_too_short <- 2

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = train_idx_too_short,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # train_idx not containing integers
      train_idx_not_integer <- c(3:5) + 0.1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = train_idx_not_integer,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # train_idx being out of range
      train_idx_out_of_range <- 1:5

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = train_idx_out_of_range,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `explain_idx`", {
  set.seed(123)

  expect_snapshot(
    {
      # explain_idx not containing integers
      explain_idx_not_integer <- c(3:5) + 0.1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = explain_idx_not_integer,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # explain_idx being out of range
      explain_idx_out_of_range <- 1:5

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = explain_idx_out_of_range,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `explain_y_lags`", {
  set.seed(123)

  expect_snapshot(
    {
      # explain_y_lags not positive
      explain_y_lags_negative <- -1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = explain_y_lags_negative,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # explain_y_lags not integer valued
      explain_y_lags_not_integer <- 2.1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = explain_y_lags_not_integer,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # explain_y_lags more than single integer
      explain_y_lags_more_than_one <- c(1, 2)

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = explain_y_lags_more_than_one,
        explain_xreg_lags = 2,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # explain_y_lags is zero for model without xreg
      explain_y_lags_zero <- 0

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp_noxreg,
        y = data_arima[1:150, "Temp"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 0,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})


test_that("erroneous input: `explain_x_lags`", {
  set.seed(123)

  expect_snapshot(
    {
      # explain_xreg_lags not positive
      explain_xreg_lags_negative <- -2

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = explain_xreg_lags_negative,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # explain_xreg_lags not integer valued
      explain_xreg_lags_not_integer <- 2.1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = explain_xreg_lags_not_integer,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # explain_x_lags wrong length
      explain_x_lags_wrong_length <- c(1, 2) # only 1 xreg variable defined

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = explain_x_lags_wrong_length,
        horizon = 3,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `horizon`", {
  set.seed(123)

  expect_snapshot(
    {
      # horizon not positive
      horizon_negative <- -2

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = horizon_negative,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # horizon not integer valued
      horizon_not_integer <- 2.1

      explain_forecast(
        testing = TRUE,
        model = model_arima_temp,
        y = data_arima[1:150, "Temp"],
        xreg = data_arima[, "Wind"],
        train_idx = 2:148,
        explain_idx = 149:150,
        explain_y_lags = 2,
        explain_xreg_lags = 2,
        horizon = horizon_not_integer,
        approach = "independence",
        phi0 = p0_ar,
        seed = 1
      )
    },
    error = TRUE
  )
})


test_that("Forecast data setup produces expected results", {
  mock_y <- matrix(1:100, 100, dimnames = list(NULL, "Y1"))
  mock_xreg <- matrix(101:205, 105, dimnames = list(NULL, "X1"))

  formatted_data <- get_data_forecast(
    mock_y,
    mock_xreg,
    train_idx = 2:99,
    explain_idx = 100,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 5
  )

  # Y1 lag 1, Y2 lag 2, X1 lag 1, X1 lag 2, X1 f1, f2, ... f5.
  x_explain <- c(100, 99, 200, 199, 201, 202, 203, 204, 205)
  expect_equal(x_explain, as.numeric(formatted_data$x_explain))

  # The data is just linearly increasing, idx 99 should be idx 100 - 1 at each value.
  expect_equal(x_explain - 1, as.numeric(formatted_data$x_train[98, ]))
})


# Tests producing output, but also other aspects below here
# These should be run on cran and are therefore placed here instead of in test-forecast-output.R


test_that("ARIMA gives the same output with different horizons", {
  h3 <- explain_forecast(
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
    phi0 = p0_ar[1:3],
    seed = 1,
    group_lags = FALSE,
    max_n_coalitions = 200,
    iterative = FALSE
  )


  h2 <- explain_forecast(
    testing = TRUE,
    model = model_arima_temp,
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
    max_n_coalitions = 100,
    iterative = FALSE
  )

  h1 <- explain_forecast(
    testing = TRUE,
    model = model_arima_temp,
    y = data_arima[1:150, "Temp"],
    xreg = data_arima[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 1,
    approach = "empirical",
    phi0 = p0_ar[1],
    seed = 1,
    group_lags = FALSE,
    max_n_coalitions = 50,
    iterative = FALSE
  )

  cols_horizon1 <- h2$internal$objects$cols_per_horizon[[1]]
  expect_equal(
    h2$shapley_values_est[horizon == 1, ..cols_horizon1],
    h1$shapley_values_est[horizon == 1, ..cols_horizon1]
  )

  expect_equal(
    h3$shapley_values_est[horizon == 1, ..cols_horizon1],
    h1$shapley_values_est[horizon == 1, ..cols_horizon1]
  )

  cols_horizon2 <- h2$internal$objects$cols_per_horizon[[2]]
  expect_equal(
    h3$shapley_values_est[horizon == 2, ..cols_horizon2],
    h2$shapley_values_est[horizon == 2, ..cols_horizon2]
  )
})

test_that("ARIMA gives the same output with different horizons with grouping", {
  h3 <- explain_forecast(
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
    phi0 = p0_ar[1:3],
    seed = 1,
    group_lags = TRUE,
    max_n_coalitions = 50,
    iterative = FALSE
  )


  h2 <- explain_forecast(
    testing = TRUE,
    model = model_arima_temp,
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
    group_lags = TRUE,
    max_n_coalitions = 50,
    iterative = FALSE
  )

  h1 <- explain_forecast(
    testing = TRUE,
    model = model_arima_temp,
    y = data_arima[1:150, "Temp"],
    xreg = data_arima[, "Wind"],
    train_idx = 2:148,
    explain_idx = 149:150,
    explain_y_lags = 2,
    explain_xreg_lags = 2,
    horizon = 1,
    approach = "empirical",
    phi0 = p0_ar[1],
    seed = 1,
    group_lags = TRUE,
    max_n_coalitions = 50,
    iterative = FALSE
  )

  expect_equal(
    h2$shapley_values_est[horizon == 1],
    h1$shapley_values_est[horizon == 1]
  )

  expect_equal(
    h3$shapley_values_est[horizon == 1],
    h1$shapley_values_est[horizon == 1]
  )

  expect_equal(
    h3$shapley_values_est[horizon == 2],
    h2$shapley_values_est[horizon == 2]
  )
})
