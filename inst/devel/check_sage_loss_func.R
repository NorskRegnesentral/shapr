library(xgboost)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]
data[, Ozone_sub30 := (Ozone < 30) * 1]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"
y_var_binary <- "Ozone_sub30"

x_train <- data[, ..x_var]
y_train <- data[, get(y_var)]
y_train_binary <- data[, get(y_var_binary)]


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

model_binary <- xgboost(
  data = as.matrix(x_train),
  label = y_train_binary,
  nround = 20,
  verbose = FALSE
)

p0 <- mean(y_train)
p0_binary <- mean(y_train_binary)

loss_func_mse <- function(response, response_hat) {
  return(colMeans((response_hat - response)^2))
}

explanation_sage_loss <- explain(
  model = model_binary,
  x_explain = x_train,
  x_train = x_train,
  approach = "gaussian",
  phi0 = p0_binary,
  sage = TRUE,
  response = y_train_binary
)

plot.shapr(explanation_sage_loss, plot_type = "waterfall")
