library(progressr)
library(future.apply)
library(xgboost)
library(shapr)
library(data.table)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus", "age", "ptratio")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
p <- mean(y_train)

#plan(multisession, workers = 4)
plan(sequential)
handlers(global = TRUE)
handlers("progress")
# Prepare the data for explanation
x <- explain(x_train, x_test, model,approach="copula", prediction_zero=p, n_batches = 4)

