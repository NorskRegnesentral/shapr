data <- data.table::as.data.table(airquality)

model_ar_temp <- ar(data$Temp, order = 2)
model_ar_temp$n.ahead <- 3

p0_ar <- rep(mean(data$Temp), 3)