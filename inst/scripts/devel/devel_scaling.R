

library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
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

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation_scaled <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,
  standardize_data= T
  )

explanation <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,
  standardize_data= F
)

# > explanation
# none    lstat        rm         dis     indus
# 1: 22.446 5.251304 -1.110224  0.47235855 4.2420117
# 2: 22.446 1.785808 -1.346290 -0.23228736 0.5987074
# 3: 22.446 5.406434  4.621065 -0.01318557 0.6551533
# 4: 22.446 5.862828  2.310974  0.59649798 2.2138450
# 5: 22.446 1.628740  5.126856  0.83973011 1.6885134
# 6: 22.446 2.540783 -2.649493  0.70016103 1.8125713
# > explanation_scaled
# none    lstat        rm         dis     indus
# 1: 22.446 5.251304 -1.110224  0.47235855 4.2420117
# 2: 22.446 1.785808 -1.346290 -0.23228736 0.5987074
# 3: 22.446 5.406434  4.621065 -0.01318557 0.6551533
# 4: 22.446 5.862828  2.310974  0.59649798 2.2138450
# 5: 22.446 1.628740  5.126856  0.83973011 1.6885134
# 6: 22.446 2.540783 -2.649493  0.70016103 1.8125713

# This currently works, except that explainer$x_train is updated by reference within explain and will therefore
# be changed. We can change it back towards the end of the function, but if the code fails one may not get there

# One possibility would be to do the initial scaling already in shapr, also allowing the user to specify the scale_list,
# as one may want to use something else (a bigger data set maybe). If all tests run fine and this always give the same
# results, we should scale by default, but let the user override this if desired.





# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)

# Finally we plot the resulting explanations
plot(explanation)
