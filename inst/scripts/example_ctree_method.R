library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

#### Example with just continuous features ####

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Just looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the ctree approach with default mincriterion = 0.95, minsplit = 20, minbucket = 7,
# and sample = TRUE
explanation <- explain(x_test, explainer,
                       approach = "ctree",
                       prediction_zero = p0)

# Printing the Shapley values for the test data
explanation$dt

# Finally we plot the resulting explanations
plot(explanation)


#### Example with mixed continuous and categorical features ####

x_train_cat <- as.data.frame(x_train)
x_test_cat <- as.data.frame(x_test)

# convert to factors for illustational purpose
x_train_cat$rm = factor(round(x_train_cat$rm))
x_test_cat$rm = factor(round(x_test_cat$rm),levels = levels(x_train_cat$rm))


# -- special function when using categorical data + xgboost
dummylist <- make_dummies(data = rbind(x_train_cat, x_test_cat))
x_train_dummy <- apply_dummies(obj = dummylist, newdata = x_train_cat)
x_test_dummy <- apply_dummies(obj = dummylist, newdata = x_test_cat)

# Fitting a basic xgboost model to the training data
model_cat <- xgboost::xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
model_cat$dummylist <- dummylist

explainer_cat <- shapr(x_test_cat, model_cat)

explanation_cat <- explain(
  x_test_cat,
  approach = "ctree",
  explainer = explainer_cat,
  prediction_zero = p0
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_cat)


