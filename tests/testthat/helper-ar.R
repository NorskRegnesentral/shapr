
data <- data.table::as.data.table(airquality)

model_ar_temp <- ar(data$Temp, order = 2)
model_ar_temp$n.ahead <- 3

x_explain_ar <- embed(data$Temp[seq.int(to=nrow(data), length.out=2)], 2)
x_train_ar <- embed(data$Temp[seq_len(nrow(data) - 2)], 2)

colnames(x_explain_ar) <- c("ar1", "ar2")
colnames(x_train_ar) <- c("ar1", "ar2")

predict(model_ar_temp, rev(t(x_explain_ar)), n.ahead = 3, se.fit = FALSE)
predict(model_ar_temp, n.ahead = 3, se.fit = FALSE)

p0_ar <- rep(mean(data$Temp), 3)
