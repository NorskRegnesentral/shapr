
# Read additional data
x_full <- fread(file.path(path_data_and_model,"x_full.csv"))
data_fit <- x_full[seq_len(729), ]

# Fit AR(2)-model
model_ar <- ar(data_fit$temp, order = 2)
phi0_ar <- rep(mean(data_fit$temp), 3)

exp_fc_ar <- explain_forecast(
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
print(exp_fc_ar)

# Fit ARIMA(2,0,0)-model
model_arimax <- arima(data_fit$temp, order = c(2, 0, 0), xreg = data_fit$windspeed)
phi0_arimax <- rep(mean(data_fit$temp), 2)

exp_fc_arimax <- explain_forecast(
  model = model_arimax,
  y = x_full[, "temp"],
  xreg = x_full[, "windspeed"],
  train_idx = 2:728,
  explain_idx = 729,
  explain_y_lags = 2,
  explain_xreg_lags = 1,
  horizon = 2,
  approach = "empirical",
  phi0 = phi0_arimax,
  group_lags = TRUE
)

print(exp_fc_arimax)
