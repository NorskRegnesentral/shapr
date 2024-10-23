
library(xgboost)
library(data.table)
library(shapr)

path <- "inst/code_paper/"
x_full <- fread(paste0(path, "x_full.csv"))


model_ar <- ar(x_full$temp, order = 2)

phi0_ar <- rep(mean(x_full$temp), 3)

explain_forecast(
  model = model_ar,
  y = x_full[, "temp"],
  train_idx = 2:729,
  explain_idx = 730:731,
  explain_y_lags = 2,
  horizon = 3,
  approach = "empirical",
  phi0 = phi0_ar,
  group_lags = FALSE
)


data_fit <- x_full[seq_len(729), ]
model_arimax <- arima(data_fit$temp, order = c(2, 0, 0), xreg = data_fit$windspeed)
phi0_arimax <- rep(mean(data_fit$temp), 2)

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
  phi0 = phi0_arimax,
  group_lags = TRUE
)
