library(xgboost)
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
explanation <- explain(
  x_train = x_train,
  x_explain = x_test,
  model = model,
  approach = "categorical",
  prediction_zero = p
)
#>
#> Success with message:
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$shapley_values)
#> 6: 43.08571  0.4786417 -5.248686 -12.55344  -6.645738

# Finally we plot the resulting explanations
plot(explanation)
