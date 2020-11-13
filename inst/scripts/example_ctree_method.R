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

explainer_cat <- shapr(x_train_cat, model_cat)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

explanation_cat <- explain(
  x_test_cat,
  approach = "ctree",
  explainer = explainer_cat,
  prediction_zero = p0
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_cat)


# Tests---
# 1) Train has more features than test but features in test are contained in train
x_train1 <- cbind(x_train_cat, 1)
x_test1 <- x_test_cat
make_dummies_list <- make_dummies(traindata = x_train1, testdata = x_test1) # error


# 2) Train has different feature types than test
x_train2 <- x_train_cat
x_test2 <- x_test_cat
x_test2$rm <- factor(round(x_test2$rm))
make_dummies_list <- make_dummies(traindata = x_train2, testdata = x_test2) # error


# 3) Test has more features than train but features in train are contained in test
x_train3 <- x_train_cat
x_test3 <- cbind(x_test_cat, 1)
make_dummies_list <- make_dummies(traindata = x_train3, testdata = x_test3) # error
# make_dummies_list$train_dummies
# make_dummies_list$test_dummies

# 4) Train and test only have numerical features
x_train4 <- x_train_cat
x_test4 <- x_test_cat
make_dummies_list <- make_dummies(traindata = x_train4, testdata = x_test4)

# 5) Train and test only have categorical features
library(data.table)
x_train5 <- data.table(x_train_cat)
x_train5 <- x_train5[, "rm", with = FALSE]
x_train5$rm <- factor(round(x_train5$rm))

x_test5 <- data.table(x_test_cat)
x_test5 <- x_test5[, "rm", with = FALSE]
x_test5$rm <- factor(round(x_test5$rm), levels = levels(x_train5$rm))

make_dummies_list <- make_dummies(traindata = x_train5, testdata = x_test5)

# 6) Test has the same levels as train but random ordering of levels
x_train6 <- x_train_cat
x_test6 <- x_test_cat

x_train6$rm <- factor(round(x_train6$rm))
x_test6$rm <- factor(round(x_test6$rm), levels = c(8, 9, 7, 4, 5, 6))

make_dummies_list <- make_dummies(traindata = x_train6, testdata = x_test6)


# 7) Test has different levels than train
x_train7 <- x_train_cat
x_train7$rm <- factor(round(x_train7$rm))

x_test7 <- x_test_cat
x_test7$rm <- factor(round(x_test7$rm))

make_dummies_list <- make_dummies(traindata = x_train7, testdata = x_test7) # error

# 8) Train and test have different feature names
x_train8 <- x_train_cat
x_test8 <- x_test_cat
names(x_test8) <- c("lstat2", "rm2", "dis2", "indus2")

make_dummies_list <- make_dummies(traindata = x_train8, testdata = x_test8) # error

# 9) Train has an empty feature name
x_train9 <- x_train_cat
x_test9 <- x_test_cat
names(x_train9) <- c("", "rm", "dis", "indus")
names(x_test9) <- c("", "rm", "dis", "indus")

make_dummies_list <- make_dummies(traindata = x_train9, testdata = x_test9) # error

# 10) Test has a column that repeats
x_train10 <- cbind(x_train_cat, lstat = x_train_cat$lstat)
x_test10 <- cbind(x_test_cat, lstat = x_test_cat$lstat)

make_dummies_list <- make_dummies(traindata = x_train10, testdata = x_test10)



