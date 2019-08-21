rm(list = ls())

library(shapr)
library(MASS)

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Explain predictions (empirical)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train))

# Explain predictions (gaussian)
explanation <- explain(x_test, explainer, approach = "gaussian", prediction_zero = mean(y_train))
explanation

# Explain predictions (copula)
explanation <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
explanation

# Explain predictions (combined)
explanation <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
explanation

