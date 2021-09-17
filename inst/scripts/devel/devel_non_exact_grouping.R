
### NOTE: THIS DOES NO LONGER WORK AS WE SWITCH TO exact when a large n_combinations is used, but the checks
### confirms the code works as intended.

library(xgboost)
library(shapr)
library(data.table)

#### Testing grouping function

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus","age","tax","ptratio","black","nox")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

group <- list(A=x_var[1:3],B=x_var[4:5],C=x_var[7],D=x_var[c(6,8)],E=x_var[9])

explainer1 <- shapr(x_train, model,group = group,n_combinations=10^ 6)

explainer2 <- shapr(x_train, model,group = group)

explanation1 <- explain(
  x_test,
  approach = "independence",
  explainer = explainer1,
  prediction_zero = p
)

explanation2 <- explain(
  x_test,
  approach = "independence",
  explainer = explainer2,
  prediction_zero = p
)


explanation2$dt-explanation1$dt # All small differences!


