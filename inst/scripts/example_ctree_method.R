library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

#### 1) Example with just continuous features ####

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


#### 2) Example with mixed continuous and categorical features ####
library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

x_train_cat <- as.data.frame(x_train)
x_test_cat <- as.data.frame(x_test)

# convert to factors for illustational purpose
x_train_cat$rm <- factor(round(x_train_cat$rm))
x_test_cat$rm <- factor(round(x_test_cat$rm), levels = c(8, 9, 7, 4, 5, 6))
# x_test_cat$rm <- factor(round(x_test_cat$rm)) # this won't work because different levels!

# Make sure they have the same levels!
print(levels(x_train_cat$rm))
print(levels(x_test_cat$rm))

# traindata = x_train_cat
# testdata = x_test_cat

# -- special function when using categorical data + xgboost
make_dummies_list <- make_dummies(traindata = x_train_cat, testdata = x_test_cat)

x_train_dummy <- make_dummies_list$train_dummies
x_test_dummy <- make_dummies_list$test_dummies

# Fitting a basic xgboost model to the training data
model_cat <- xgboost::xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
model_cat$dummylist <- make_dummies_list$obj

explainer_cat <- shapr(make_dummies_list$traindata_new, model_cat)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# make_dummies_list$testdata_new$rm

explanation_cat <- explain(
  make_dummies_list$testdata_new,
  approach = "ctree",
  explainer = explainer_cat,
  prediction_zero = p0
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_cat)

