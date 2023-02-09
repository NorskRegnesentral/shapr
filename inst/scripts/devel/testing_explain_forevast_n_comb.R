

h3test <- explain_forecast(model = model_arima_temp,
                                y = data[1:150, "Temp"],
                                xreg = data[, "Wind"],
                                train_idx = 2:148,
                                explain_idx = 149:150,
                                explain_y_lags = 2,
                                explain_xreg_lags = 2,
                                horizon = 3,
                                approach = "empirical",
                                prediction_zero = p0_ar[1:3],
                                group_lags = FALSE,
                                n_batches = 1,
                                timing = FALSE,
                                seed = i,
                                n_combinations = 300
)

h2test <- explain_forecast(model = model_arima_temp,
                           y = data[1:150, "Temp"],
                           xreg = data[, "Wind"],
                           train_idx = 2:148,
                           explain_idx = 149:150,
                           explain_y_lags = 2,
                           explain_xreg_lags = 2,
                           horizon = 2,
                           approach = "empirical",
                           prediction_zero = p0_ar[1:2],
                           group_lags = FALSE,
                           n_batches = 1,
                           timing = FALSE,
                           seed = i,
                           n_combinations = 10^7
)

h1test <- explain_forecast(model = model_arima_temp,
                           y = data[1:150, "Temp"],
                           xreg = data[, "Wind"],
                           train_idx = 2:148,
                           explain_idx = 149:150,
                           explain_y_lags = 2,
                           explain_xreg_lags = 2,
                           horizon = 1,
                           approach = "empirical",
                           prediction_zero = p0_ar[1],
                           group_lags = FALSE,
                           n_batches = 1,
                           timing = FALSE,
                           seed = i,
                           n_combinations = 10^7
)

w <- h3test$internal$objects$X_list[[1]][["shapley_weight"]]

w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
h3test$internal$objects$X_list[[1]][,shapley_weight_norm := w]


w <- h1test$internal$objects$X_list[[1]][["shapley_weight"]]

w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
h1test$internal$objects$X_list[[1]][,shapley_weight_norm := w]


w2 <- h1full$internal$objects$X_list[[1]][["shapley_weight"]]

w2[-c(1, length(w2))] <- w2[-c(1, length(w2))] / sum(w2[-c(1, length(w2))])
h1full$internal$objects$X_list[[1]][,shapley_weight_norm := w2]

h1test$internal$objects$X_list[[1]][-c(1,.N),]
h1full$internal$objects$X_list[[1]][-c(1,.N),]
h3test$internal$objects$X_list[[1]][-c(1,.N),]


ncomb <- 50

reps <- 10

set.seed(123)
h3full <- explain_forecast(model = model_arima_temp,
                       y = data[1:150, "Temp"],
                       xreg = data[, "Wind"],
                       train_idx = 2:148,
                       explain_idx = 149:150,
                       explain_y_lags = 2,
                       explain_xreg_lags = 2,
                       horizon = 3,
                       approach = "empirical",
                       prediction_zero = p0_ar[1:3],
                       group_lags = FALSE,
                       n_batches = 1,
                       timing = FALSE,
                       seed = 1)

set.seed(123)
h1full <- explain_forecast(model = model_arima_temp,
                           y = data[1:150, "Temp"],
                           xreg = data[, "Wind"],
                           train_idx = 2:148,
                           explain_idx = 149:150,
                           explain_y_lags = 2,
                           explain_xreg_lags = 2,
                           horizon = 1,
                           approach = "empirical",
                           prediction_zero = p0_ar[1],
                           group_lags = FALSE,
                           n_batches = 1,
                           timing = FALSE,
                           seed = 1)



h1list <- h2list <- h3list <- list()
for (i in 1:reps){
  h3list[[i]] <- explain_forecast(model = model_arima_temp,
                         y = data[1:150, "Temp"],
                         xreg = data[, "Wind"],
                         train_idx = 2:148,
                         explain_idx = 149:150,
                         explain_y_lags = 2,
                         explain_xreg_lags = 2,
                         horizon = 3,
                         approach = "empirical",
                         prediction_zero = p0_ar[1:3],
                         group_lags = FALSE,
                         n_batches = 1,
                         timing = FALSE,
                         seed = i,
                         n_combinations = ncomb
  )

  h2list[[i]] <- explain_forecast(model = model_arima_temp,
                         y = data[1:150, "Temp"],
                         xreg = data[, "Wind"],
                         train_idx = 2:148,
                         explain_idx = 149:150,
                         explain_y_lags = 2,
                         explain_xreg_lags = 2,
                         horizon = 2,
                         approach = "empirical",
                         prediction_zero = p0_ar[1:2],
                         group_lags = FALSE,
                         n_batches = 1,
                         timing = FALSE,
                         seed = i,
                         n_combinations = ncomb
  )

  h1list[[i]] <- explain_forecast(model = model_arima_temp,
                         y = data[1:150, "Temp"],
                         xreg = data[, "Wind"],
                         train_idx = 2:148,
                         explain_idx = 149:150,
                         explain_y_lags = 2,
                         explain_xreg_lags = 2,
                         horizon = 1,
                         approach = "empirical",
                         prediction_zero = p0_ar[1],
                         group_lags = FALSE,
                         n_batches = 1,
                         timing = FALSE,
                         seed = i,
                         n_combinations = min(ncomb,31)
  )

  print(i)
}



cols_horizon1 <- h3full$internal$objects$cols_per_horizon[[1]]
cols_horizon2 <- h3full$internal$objects$cols_per_horizon[[2]]
cols_horizon3 <- h3full$internal$objects$cols_per_horizon[[3]]

h1mean1 <- h2mean1 <- h2mean2 <- h3mean1 <- h3mean2 <- h3mean3 <-  list()
for(i in 1:reps){
  h1mean1[[i]] <- as.matrix(h1list[[i]]$shapley_values[horizon==1, ..cols_horizon1])

  h2mean1[[i]] <- as.matrix(h2list[[i]]$shapley_values[horizon==1, ..cols_horizon1])
  h2mean2[[i]] <- as.matrix(h2list[[i]]$shapley_values[horizon==2, ..cols_horizon2])

  h3mean1[[i]] <- as.matrix(h3list[[i]]$shapley_values[horizon==1, ..cols_horizon1])
  h3mean2[[i]] <- as.matrix(h3list[[i]]$shapley_values[horizon==2, ..cols_horizon2])
  h3mean3[[i]] <- as.matrix(h3list[[i]]$shapley_values[horizon==3, ..cols_horizon3])

}

# Horizon 1
Reduce("+", h1mean1) / reps
Reduce("+", h2mean1) / reps
Reduce("+", h3mean1) / reps
h3full$shapley_values[horizon==1,..cols_horizon1]

# Horizon 2
Reduce("+", h2mean2) / reps
Reduce("+", h3mean2) / reps
h3full$shapley_values[horizon==2,..cols_horizon2]

# Horizon 3
Reduce("+", h3mean3) / reps
h3full$shapley_values[horizon==3,..cols_horizon3]



expect_equal(h2$shapley_values[horizon==1, ..cols_horizon1],
             h1$shapley_values[horizon==1,..cols_horizon1])

expect_equal(h3$shapley_values[horizon==1, ..cols_horizon1],
             h1$shapley_values[horizon==1,..cols_horizon1])

cols_horizon2 <- h2$internal$objects$cols_per_horizon[[2]]
expect_equal(h3$shapley_values[horizon==2, ..cols_horizon2],
             h2$shapley_values[horizon==2,..cols_horizon2])
