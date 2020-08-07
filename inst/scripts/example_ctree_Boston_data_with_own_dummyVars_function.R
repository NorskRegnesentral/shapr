library(xgboost)
library(shapr)

source("inst/scripts/make_dummies_no_class.R")

data("Boston", package = "MASS")

x_var <- c("lstat", "chas", "rad", "indus")
y_var <- "medv"

# convert to factors
Boston$rad = as.factor(Boston$rad)
Boston$chas = as.factor(Boston$chas)

x_train <- Boston[-1:-6, x_var]
y_train <- Boston[-1:-6, y_var]
x_test <- Boston[1:6, x_var]

dummyfunc_original <- make_dummies(data = rbind(x_train, x_test))

x_train_dummies <- apply_dummies(obj = dummyfunc_original, newdata = x_train)
x_test_dummies <- apply_dummies(obj = dummyfunc_original, newdata = x_test)

# model_cont <- xgboost(
#   data = as.matrix(x_train),
#   label = y_train,
#   nround = 20,
#   verbose = FALSE
# )
# explainer_cont <- shapr(x_train, model_cont)

model_cat <- xgboost(
  data = x_train_dummies,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

model_cat$dummylist <- dummyfunc_original # this is very important! and must be called dummylist to work with the R code!
explainer_cat <- shapr(x_train, model_cat)

p <- mean(y_train)

explanation_ctree <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer_cat,
  prediction_zero = p
)

## TESTS
source("inst/scripts/make_dummies_no_class.R")

data("Boston", package = "MASS")

x_var <- c("lstat", "chas", "rad", "indus")
y_var <- "medv"

# convert to factors
Boston$rad = as.factor(Boston$rad)
Boston$chas = as.factor(Boston$chas)

x_train <- Boston[-1:-6, x_var]
y_train <- Boston[-1:-6, y_var]
x_test <- Boston[1:6, x_var]

dummyfunc_original <- make_dummies(data = rbind(x_train, x_test))

head(x_train)
original <- predict(dummyfunc_original, newdata = x_train)
head(original)

head(x_test)
original_test <- predict(dummyfunc_original, newdata = x_test)
head(original_test)

## If you re-arrange the columns, predict() will arrange them to the original order
x_train0 <- x_train[, c(2, 1, 4, 3)]
head(x_train0)
head(x_train)
diff_column_placements <- predict(dummyfunc_original, newdata = x_train0)
head(diff_column_placements)

## What if you have really bad feature names like X1 and X11?
new_names <- x_train0
colnames(new_names) <- c("X1", "X11", "indus", "rad")

head(new_names)
dummyfunc_bad_column_names <- make_dummies(" ~ .", data = new_names)

bad_column_names <- predict(dummyfunc_bad_column_names, newdata = new_names)
head(bad_column_names)

## What if you put in less features then the original feature vector?
x_train1 <- x_train[, c(2, 1)]
head(x_train1)
head(x_train)
less_variables <- predict(dummyfunc_original, newdata = x_train1) # will cause an error
head(less_variables)

## What if you add a feature?
x_train2 <- cbind(x_train[, c(1, 2)], new_var = x_train[,2], x_train[, c(3, 4)])
head(x_train2)
head(x_train)
a_new_var <- predict(dummyfunc_original, newdata = x_train2) # will not throw an error - do we want it to throw an error?
head(a_new_var)

## What if you have two variables with the same name?
x_train3 <- x_train
colnames(x_train3) <- c("X1", "X2", "X3", "X3")
head(x_train3)
dummyfunc_same_var_name <- make_dummies(data = rbind(x_train3)) # will throw an error


## What if variables don't have names?
x_train3 <- x_train
colnames(x_train3) <- c("", "", "", "")
head(x_train3)
dummyfunc_no_var_names <- make_dummies(data = rbind(x_train3)) # will throw an error

# library(caret)
# # -- special function when using categorical data + xgboost
# dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test))
# x_train_dummy <- predict(dummyfunc, newdata = x_train)
# x_test_dummy <- predict(dummyfunc, newdata = x_test)
#
# model_cat <- xgboost(
#   data = x_train_dummy,
#   label = y_train,
#   nround = 20,
#   verbose = FALSE
# )
#
# model_cat$dummyfunc <- dummyfunc
#
# explainer_cat <- shapr(x_train, model_cat)
#
# p <- mean(y_train)
#
# explanation_ctree <- explain(
#   x_test,
#   approach = "ctree",
#   explainer = explainer_cat,
#   prediction_zero = p
# )
