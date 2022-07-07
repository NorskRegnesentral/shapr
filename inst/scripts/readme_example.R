library(xgboost)
library(shapr)

data("airquality")
airquality <- airquality[complete.cases(airquality), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_test <- 1:6
x_train <- as.matrix(airquality[-ind_x_test, x_var])
y_train <- airquality[-ind_x_test, y_var]
x_test <- as.matrix(airquality[ind_x_test, x_var])

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_train,
  x_test,
  model = model,
  approach = "empirical",
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$shapley_values)

# Finally we plot the resulting explanations
plot(explanation)
