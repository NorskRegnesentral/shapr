library(MASS)
library(xgboost)
library(shapr)
library(ggplot2)
library(data.table)

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Just looking at the dependence between the features

cor(x_train)
# > cor(x_train)
# lstat         rm        dis      indus
# lstat  1.0000000 -0.6106362 -0.5075796  0.6073291
# rm    -0.6106362  1.0000000  0.2051866 -0.3897134
# dis   -0.5075796  0.2051866  1.0000000 -0.7059103
# indus  0.6073291 -0.3897134 -0.7059103  1.0000000


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)


# Prepare the data for explanation
l <- prepare_kshap(
  xtrain = x_train,
  xtest = x_test
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
explanation$kshap

#      none     lstat         rm       dis      indus
# 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
# 2: 22.446 0.1671903 -0.7088405 0.9689007  0.3786871
# 3: 22.446 5.9888016  5.5450861 0.5660136 -1.4304350
# 4: 22.446 8.2142203  0.7507569 0.1893368  1.8298305
# 5: 22.446 0.5059890  5.6875106 0.8432240  2.2471152
# 6: 22.446 1.9929674 -3.6001959 0.8601984  3.1510531

# Finally we plot the resulting explanations
plot_kshap(explanation, l)
