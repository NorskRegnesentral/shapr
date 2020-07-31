
library(caret)
library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var_cat <- c("lstat", "chas", "rad", "indus")
y_var <- "medv"

# convert to factors
Boston$rad = as.factor(Boston$rad)
Boston$chas = as.factor(Boston$chas)

x_train_cat <- Boston[-1:-6, x_var_cat]
y_train <- Boston[-1:-6, y_var]
x_test_cat <- Boston[1:6, x_var_cat]

# -- special function when using categorical data + xgboost
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train_cat, x_test_cat))
x_train_dummy <- predict(dummyfunc, newdata = x_train_cat)
x_test_dummy <- predict(dummyfunc, newdata = x_test_cat)
# --

# Fitting a basic xgboost model to the training data
model_cat <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
model_cat$dummyfunc <- dummyfunc

explainer_cat <- shapr(x_train_cat, model_cat)

p <- mean(y_train)

explanation_ctree <- explain(
  x_test_cat,
  approach = "ctree",
  explainer = explainer_cat,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_ctree, plot_phi0 = FALSE, index_x_test = c(1, 6))
