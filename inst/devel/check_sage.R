library(xgboost)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

x_train <- data[, ..x_var]
y_train <- data[, get(y_var)]


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

p0 <- mean(y_train)

explanation_sage <- explain(
  model = model,
  x_explain = x_train,
  x_train = x_train,
  approach = "gaussian",
  phi0 = p0,
  sage = TRUE,
  response = y_train,
  iterative = TRUE
)

plot.shapr(explanation_sage, plot_type = "waterfall")
