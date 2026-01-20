# Test that all supported model classes work with explain/explain_forecast
# These are simple smoke tests to ensure upstream package changes don't break shapr

# lm model (stats - always available)
test_that("lm model works with explain", {
  # Fit model
  data_train <- cbind(Ozone = y_train_reg, x_train_reg)
  model <- lm(Ozone ~ ., data = data_train)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4) # 2 features + none
})


# glm model - regression (stats - always available)
test_that("glm model works with explain (gaussian)", {
  # Fit model
  data_train <- cbind(Ozone = y_train_reg, x_train_reg)
  model <- glm(Ozone ~ ., data = data_train, family = gaussian())

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# glm model - classification (stats - always available)
test_that("glm model works with explain (binomial)", {
  # Fit model
  data_train <- cbind(Ozone_binary = y_train_class, x_train_class)
  model <- glm(Ozone_binary ~ ., data = data_train, family = binomial())

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_class,
    x_train = x_train_class,
    approach = "independence",
    phi0 = phi0_class,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# ar model (stats)
test_that("ar model works with explain_forecast", {
  skip_if_not_installed("forecast") # For prediction

  # Fit AR model
  model <- ar(y_ts, order.max = 2, method = "ols")
  model$n_ahead <- 1

  # Run explain_forecast
  explanation <- explain_forecast(
    model = model,
    y = y_ts,
    train_idx = train_idx_ts,
    explain_idx = explain_idx_ts,
    explain_y_lags = 2,
    horizon = 1,
    approach = "independence",
    phi0 = phi0_ts,
    seed = 123,
    group_lags = FALSE,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
})

# Arima model (stats)
test_that("Arima (stats) model works with explain_forecast", {
  skip_if_not_installed("forecast") # For prediction

  # Fit AR model
  model <- arima(y_ts, order = c(2, 1, 0))
  model$n_ahead <- 1

  # Run explain_forecast
  explanation <- explain_forecast(
    model = model,
    y = y_ts,
    train_idx = train_idx_ts,
    explain_idx = explain_idx_ts,
    explain_y_lags = 2,
    horizon = 1,
    approach = "independence",
    phi0 = phi0_ts,
    seed = 123,
    group_lags = FALSE,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
})

# Arima model (forecast)
test_that("Arima (forecast) model works with explain_forecast", {
  skip_if_not_installed("forecast")

  # Fit AR model
  model <- forecast::Arima(y_ts, order = c(2, 1, 0))
  model$n_ahead <- 1

  # Run explain_forecast
  explanation <- explain_forecast(
    model = model,
    y = y_ts,
    train_idx = train_idx_ts,
    explain_idx = explain_idx_ts,
    explain_y_lags = 2,
    horizon = 1,
    approach = "independence",
    phi0 = phi0_ts,
    seed = 123,
    group_lags = FALSE,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
})

# ranger model - regression (conditional)
test_that("ranger model works with explain (regression)", {
  skip_if_not_installed("ranger")

  # Fit model
  data_train <- cbind(Ozone = y_train_reg, x_train_reg)
  model <- ranger::ranger(Ozone ~ ., data = data_train, num.trees = 50)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# ranger model - classification (conditional)
test_that("ranger model works with explain (probability)", {
  skip_if_not_installed("ranger")

  # Fit model with probability = TRUE
  data_train <- cbind(Ozone_binary = y_train_class, x_train_class)
  model <- ranger::ranger(Ozone_binary ~ ., data = data_train, num.trees = 50, probability = TRUE)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_class,
    x_train = x_train_class,
    approach = "independence",
    phi0 = phi0_class,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# xgboost model - regression (conditional)
test_that("xgb.train model works with explain (regression)", {
  skip_if_not_installed("xgboost")

  # Fit model
  dtrain <- xgboost::xgb.DMatrix(x_train_reg, label = y_train_reg)
  model <- xgboost::xgb.train(
    data = dtrain,
    nrounds = 10,
    verbose = 0,
    params = list(
      objective = "reg:squarederror",
      verbosity = 0
    )
  )

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# xgboost model - classification (conditional)
test_that("xgb.train model works with explain (binary classification)", {
  skip_if_not_installed("xgboost")

  # Fit model
  dtrain <- xgboost::xgb.DMatrix(x_train_class, label = as.numeric(y_train_class) - 1)
  model <- xgboost::xgb.train(
    data = dtrain,
    nrounds = 10,
    params = list(
      objective = "binary:logistic", # equivalent to "binary:logistic"
      verbosity = 0
    )
  )

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_class,
    x_train = x_train_class,
    approach = "independence",
    phi0 = phi0_class,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# xgboost wrapper - regression (conditional)
test_that("xgboost wrapper works with explain (regression)", {
  skip_if_not_installed("xgboost")

  # Fit model using xgboost() wrapper
  model <- xgboost::xgboost(
    x = x_train_reg,
    y = y_train_reg,
    nrounds = 10,
    verbosity = 0,
    objective = "reg:squarederror"
  )

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# xgboost wrapper - classification (conditional)
test_that("xgboost wrapper works with explain (binary classification)", {
  skip_if_not_installed("xgboost")

  # Fit model using xgboost() wrapper
  model <- xgboost::xgboost(
    x = x_train_class,
    y = y_train_class,
    nrounds = 10,
    verbosity = 0,
    objective = "binary:logistic"
  )

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_class,
    x_train = x_train_class,
    approach = "independence",
    phi0 = phi0_class,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# mgcv gam model (conditional)
test_that("mgcv gam model works with explain", {
  skip_if_not_installed("mgcv")

  # Fit model
  data_train <- cbind(Ozone = y_train_reg, x_train_reg)
  model <- mgcv::gam(Ozone ~ s(Solar.R) + Wind, data = data_train)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})


# workflows (tidymodels) model (conditional)
test_that("workflows model works with explain", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")

  # Build workflow
  data_train <- cbind(Ozone = y_train_reg, x_train_reg)
  recipe <- recipes::recipe(Ozone ~ ., data = data_train)
  lm_model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm") |>
    parsnip::set_mode("regression")

  model <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(lm_model) |>
    parsnip::fit(data = data_train)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain_reg,
    x_train = x_train_reg,
    approach = "independence",
    phi0 = phi0_reg,
    seed = 123,
    verbose = NULL
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values_est), 2)
  expect_equal(ncol(explanation$shapley_values_est), 4)
})
