library(shapr)
library(MASS)

devtools::load_all()

# ------------------------------

Boston$rad <- as.factor(Boston$rad)
Boston$chas <- as.factor(Boston$chas)
x_var <- c("rad", "chas")
y_var <- "medv"

ind_x_test <- 1:6
train <- Boston[-ind_x_test, c(x_var, y_var)]
x_test <- Boston[ind_x_test, x_var]
x_train = train[, x_var]

# Fitting a basic xgboost model to the training data
model <- lm(medv ~ rad + chas, data = train)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(train$medv)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)

explain(
  x_train = x_train,
  x_explain = x_test,
  model = model,
  approach = "categorical", #  "ctree",
  prediction_zero = p
)

