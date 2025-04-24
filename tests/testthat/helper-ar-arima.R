options(digits = 5) # To avoid round off errors when printing output on different systems

data_arima <- data.table::as.data.table(airquality)
data_arima[, Solar.R := ifelse(is.na(Solar.R), mean(Solar.R, na.rm = TRUE), Solar.R)]
data_arima[, Ozone := ifelse(is.na(Ozone), mean(Ozone, na.rm = TRUE), Ozone)]

model_ar_temp <- ar(data_arima$Temp, order = 2)
model_ar_temp$n.ahead <- 3

p0_ar <- rep(mean(data_arima$Temp), 3)

model_arima_temp <- arima(data_arima$Temp[1:150], c(2, 1, 0), xreg = data_arima$Wind[1:150])
model_arima_temp2 <- arima(data_arima$Temp[1:150], c(2, 1, 0), xreg = data_arima[1:150, c("Wind", "Solar.R", "Ozone")])

model_arima_temp_noxreg <- arima(data_arima$Temp[1:150], c(2, 1, 0))

# When loading this here we avoid the "Registered S3 method overwritten" when calling forecast
if (!requireNamespace("forecast", quietly = TRUE)) {
  message("The forecast package is required for testing explain_forecast()")
} else {
  model_forecast_ARIMA_temp <- forecast::Arima(
    data_arima$Temp[1:150],
    order = c(2, 1, 0),
    xreg = data_arima$Wind[1:150]
  )
}
