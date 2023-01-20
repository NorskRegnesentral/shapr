data <- data.table::as.data.table(airquality)

model_ar_temp <- ar(data$Temp, order = 2)
model_ar_temp$n.ahead <- 3

p0_ar <- rep(mean(data$Temp), 3)

model_arima_temp <- arima(data$Temp[1:150], c(2,1,0), xreg=data$Wind[1:150])