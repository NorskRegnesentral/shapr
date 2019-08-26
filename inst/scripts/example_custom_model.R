rm(list = ls())

library(MASS)
library(caret)
library(shapr)
library(ggplot2)
library(data.table)

# Load data
data("Boston")

# Create test- and training data
x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Fitting a gbm model using caret
set.seed(825)
model <- train(
  x_train,
  y = y_train,
  method = "gbm",
  verbose = FALSE
)

# Create custom function
predict_model.train <- function(x, newdata) {

    if (!requireNamespace('caret', quietly = TRUE)) {
      stop('The caret package is required for predicting train models')
    }

    predict(x, newdata)
}

# Prepare the data for explanation
l <- prepare_kshap(
  Xtrain = x_train,
  Xtest = x_test
)

# Spedifying the phi_0, i.e. the expected prediction without any features
pred_zero <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero
)

# Printing the Shapley values for the test data
explanation$Kshap
