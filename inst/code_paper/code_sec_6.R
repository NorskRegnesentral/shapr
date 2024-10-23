devtools::load_all()

bike <- fread("inst/extdata/day.csv")

model_ar <- ar(bike$temp, order = 2)

p0_ar <- rep(mean(bike$temp), 3)

explain_forecast(
  model = model_ar,
  y = bike[, "temp"],
  train_idx = 2:729,
  explain_idx = 730:731,
  explain_y_lags = 2,
  horizon = 3,
  approach = "empirical",
  prediction_zero = p0_ar,
  group_lags = FALSE
)


data_fit <- bike[seq_len(729), ]
model_arimax <- arima(data_fit$temp, order = c(2, 0, 0), xreg = data_fit$windspeed)
p0_arimax <- rep(mean(data_fit$temp), 2)

explain_forecast(
  model = model_arimax,
  y = data_fit[, "temp"],
  xreg = bike[, "windspeed"],
  train_idx = 2:728,
  explain_idx = 729,
  explain_y_lags = 2,
  explain_xreg_lags = 1,
  horizon = 2,
  approach = "empirical",
  prediction_zero = p0_arimax,
  group_lags = TRUE
)
