rm(list = ls())

library(caret)
library(shapr)

# Load data
data("Boston", package = "MASS")

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

# Create custom function of model_type for caret
model_type.train <- function(x) {
  ifelse(
    x$modelType[[1]] == "Classification",
    "classification",
    "regression"
  )
}

# Create custom function of predict_model for caret
predict_model.train <- function(x, newdata) {

  if (!requireNamespace('caret', quietly = TRUE)) {
    stop('The caret package is required for predicting train models')
  }
  model_type <- model_type(x)

  if (model_type == "classification") {

    predict(x, newdata, type = "prob")
  } else {

    predict(x, newdata)
  }
}

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)

# Printing the Shapley values for the test data
explanation$dt

# Plot results
plot(explanation)
