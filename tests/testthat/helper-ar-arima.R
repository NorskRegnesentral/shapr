options(digits = 5) # To avoid round off errors when printing output on different systems



data <- data.table::as.data.table(airquality)

model_ar_temp <- ar(data$Temp, order = 2)
model_ar_temp$n.ahead <- 3

p0_ar <- rep(mean(data$Temp), 3)

model_arima_temp <- arima(data$Temp[1:150], c(2, 1, 0), xreg = data$Wind[1:150])

model_arima_temp_noxreg <- arima(data$Temp[1:150], c(2, 1, 0))

# When loading this here we avoid the "Registered S3 method overwritten" when calling forecast
model_forecast_ARIMA_temp <- forecast::Arima(data$Temp[1:150], order = c(2, 1, 0), xreg = data$Wind[1:150])
