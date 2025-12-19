# Test that all supported model classes work with explain/explain_forecast
# These are simple smoke tests to ensure upstream package changes don't break shapr

# lm model (stats - always available) ================================================
test_that("lm model works with explain", {
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = rnorm(15),
    x1 = rnorm(15),
    x2 = rnorm(15),
    x3 = rnorm(15)
  )
  x_train <- data[1:13, -1]
  x_explain <- data[14:15, -1]

  # Fit model
  model <- lm(y ~ ., data = data[1:13, ])
  phi0 <- mean(data$y[1:13])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4) # 3 features + none
})


# glm model - regression (stats - always available) ==================================
test_that("glm model works with explain (gaussian)", {
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = rnorm(15),
    x1 = rnorm(15),
    x2 = rnorm(15),
    x3 = rnorm(15)
  )
  x_train <- data[1:13, -1]
  x_explain <- data[14:15, -1]

  # Fit model
  model <- glm(y ~ ., data = data[1:13, ], family = gaussian())
  phi0 <- mean(data$y[1:13])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# glm model - classification (stats - always available) ==============================
test_that("glm model works with explain (binomial)", {
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = factor(sample(0:1, 20, replace = TRUE)),
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  x_train <- data[1:18, -1]
  x_explain <- data[19:20, -1]

  # Fit model
  model <- glm(y ~ ., data = data[1:18, ], family = binomial())
  phi0 <- mean(as.numeric(data$y[1:18]) - 1)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# ar model (stats - always available) ================================================
test_that("ar model works with explain_forecast", {
  skip_if_not_installed("forecast")
  set.seed(123)

  # Create simple time series
  set.seed(123)
y <- arima.sim(list(ar = c(0.6, 0.3)), n = 30) + rnorm(30, 0, 0.1)
  dat <- data.table(y = y)

  # Fit AR model
  model <- ar(dat$y, order = 2)
  model$n.ahead <- 2

  # Run explain_forecast
  explanation <- explain_forecast(
    model = model,
    y = dat[, "y"],
    train_idx = 2:25,
    explain_idx = 26:28,
    explain_y_lags = 2,
    horizon = 1,
    approach = "independence",
    seed = 1,
    phi0 = mean(y[1:25]),
    group_lags = FALSE
  )


    data_arima <- data.table::as.data.table(airquality)
    data_arima[, Solar.R := ifelse(is.na(Solar.R), mean(Solar.R, na.rm = TRUE), Solar.R)]
    data_arima[, Ozone := ifelse(is.na(Ozone), mean(Ozone, na.rm = TRUE), Ozone)]
    data_arima[, y  := Temp]
    model_ar_temp <- ar(data_arima$y, order = 2)
    model_ar_temp$n.ahead <- 3


    explain_forecast(
      model = model_ar_temp,
      y = data_arima[, "y"],
      train_idx = 2:151,
      explain_idx = 152:153,
      explain_y_lags = 2,
      horizon = 3,
      approach = "empirical",
      phi0 = p0_ar,
      seed = 1,
      group_lags = FALSE
    )



  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 3)
})


# ranger model - regression (conditional) ============================================
test_that("ranger model works with explain (regression)", {
  skip_if_not_installed("ranger")
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  x_train <- data[1:18, -1]
  x_explain <- data[19:20, -1]

  # Fit model
  model <- ranger::ranger(y ~ ., data = data[1:18, ], num.trees = 50)
  phi0 <- mean(data$y[1:18])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# ranger model - classification (conditional) ========================================
test_that("ranger model works with explain (probability)", {
  skip_if_not_installed("ranger")
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = factor(sample(0:1, 30, replace = TRUE)),
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )
  x_train <- data[1:28, -1]
  x_explain <- data[29:30, -1]

  # Fit model with probability = TRUE
  model <- ranger::ranger(y ~ ., data = data[1:28, ], num.trees = 50, probability = TRUE)
  phi0 <- mean(as.numeric(data$y[1:28]) - 1)

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# xgboost model - regression (conditional) ===========================================
test_that("xgboost model works with explain (regression)", {
  skip_if_not_installed("xgboost")
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  x_train <- data[1:18, -1]
  x_explain <- data[19:20, -1]

  # Fit model
  dtrain <- xgboost::xgb.DMatrix(as.matrix(x_train), label = data$y[1:18])
  model <- xgboost::xgb.train(
    data = dtrain,
    nrounds = 10,
    verbose = 0,
    params = list(objective = "reg:squarederror")
  )
  phi0 <- mean(data$y[1:18])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# xgboost model - classification (conditional) =======================================
test_that("xgboost model works with explain (binary classification)", {
  skip_if_not_installed("xgboost")
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = sample(0:1, 30, replace = TRUE),
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )
  x_train <- data[1:28, -1]
  x_explain <- data[29:30, -1]

  # Fit model
  dtrain <- xgboost::xgb.DMatrix(as.matrix(x_train), label = data$y[1:28])
  model <- xgboost::xgb.train(
    data = dtrain,
    nrounds = 10,
    verbose = 0,
    params = list(objective = "binary:logistic")
  )
  phi0 <- mean(data$y[1:28])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# mgcv gam model (conditional) =======================================================
test_that("mgcv gam model works with explain", {
  skip_if_not_installed("mgcv")
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  x_train <- data[1:18, -1]
  x_explain <- data[19:20, -1]

  # Fit model
  model <- mgcv::gam(y ~ s(x1) + x2 + x3, data = data[1:18, ])
  phi0 <- mean(data$y[1:18])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# workflows (tidymodels) model (conditional) =========================================
test_that("workflows model works with explain", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  set.seed(123)

  # Create minimal data
  data <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  x_train <- data[1:18, -1]
  x_explain <- data[19:20, -1]

  # Build workflow
  recipe <- recipes::recipe(y ~ ., data = data[1:18, ])
  lm_model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    parsnip::set_mode("regression")

  model <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(lm_model) %>%
    parsnip::fit(data = data[1:18, ])

  phi0 <- mean(data$y[1:18])

  # Run explain
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 2)
  expect_equal(ncol(explanation$shapley_values), 4)
})


# forecast Arima model (conditional) =================================================
test_that("forecast Arima model works with explain_forecast", {
  skip_if_not_installed("forecast")
  set.seed(123)

  # Create simple time series
  y <- rnorm(30)

  # Fit ARIMA model
  model <- forecast::Arima(y, order = c(1, 0, 0))

  # Run explain_forecast
  explanation <- explain_forecast(
    model = model,
    y = y,
    train_idx = 1:25,
    explain_idx = 26:28,
    explain_y_lags = 1,
    horizon = 1,
    approach = "independence",
    seed = 123
  )

  # Basic checks
  expect_s3_class(explanation, "shapr")
  expect_equal(nrow(explanation$shapley_values), 3)
})
