library(xgboost)
library(shapr)

# 1. Mix between categorical + numerical data
data("Boston", package = "MASS")

x_var <- c("lstat", "chas", "rad", "indus")
y_var <- "medv"


# convert to factors
Boston$rad <- as.factor(Boston$rad)
Boston$chas <- as.factor(Boston$chas)

df <- Boston[-1:-6, c(x_var, y_var)]
x_train <- Boston[-1:-6, x_var]
y_train <- Boston[-1:-6, y_var]
x_test <- Boston[1:6, x_var]

dummylist <- make_dummies(data = rbind(x_train, x_test))

x_train_dummies <- apply_dummies(obj = dummylist, newdata = x_train)
x_test_dummies <- apply_dummies(obj = dummylist, newdata = x_test)

model_cat <- xgboost(
  data = x_train_dummies,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# this is very important! and must be called dummylist to work with the R code!
model_cat$dummylist <- dummylist
explainer_cat <- shapr(x_train, model_cat)

p <- mean(y_train)

explanation_ctree <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer_cat,
  prediction_zero = p
)

head(explanation_ctree$dt)
#      none      lstat        chas       rad     indus
# 1: 22.446  1.5592494 -0.26105334 0.6362008  1.500910
# 2: 22.446  0.5237327 -0.01223444 1.0819028 -2.020305
# 3: 22.446 13.7668894 -0.25545215 1.3729573 -1.232925
# 4: 22.446 14.2019475 -0.33976648 2.1820143  5.721249
# 5: 22.446  1.7482355 -0.18688633 3.9573665  3.265200
# 6: 22.446  1.6255485 -0.09387694 3.9198322  3.332412

## TESTS
dummylist <- make_dummies(data = rbind(x_train, x_test))

head(x_train)
original <- apply_dummies(dummylist, newdata = x_train)
head(original)

head(x_test)
original_test <- apply_dummies(dummylist, newdata = x_test)
head(original_test)

## If you re-arrange the columns, apply_dummies() will arrange them to the original order
x_train0 <- x_train[, c(2, 1, 4, 3)]
head(x_train0)
head(x_train)
diff_column_placements <- apply_dummies(dummylist, newdata = x_train0)
head(diff_column_placements)

## If you have different feature data types as originally passed
x_train_num <- sapply(x_train, as.numeric)
x_train_diff_data_types <- apply_dummies(obj = dummylist, newdata = x_train_num)


## What if you have really bad feature names like X1 and X11?
new_names <- x_train0
colnames(new_names) <- c("X1", "X11", "indus", "rad")

head(new_names)
dummylist_bad_column_names <- make_dummies(" ~ .", data = new_names)

bad_column_names <- apply_dummies(dummylist_bad_column_names, newdata = new_names)
head(bad_column_names)

## What if you put in less features then the original feature vector?
x_train1 <- x_train[, c(2, 1)]
head(x_train1)
head(x_train)
less_variables <- apply_dummies(dummylist, newdata = x_train1) # will cause an error
head(less_variables)

## What if you add a feature?
x_train2 <- cbind(x_train[, c(1, 2)], new_var = x_train[, 2], x_train[, c(3, 4)])
head(x_train2)
head(x_train)
# will not throw an error - do we want it to throw an error?
a_new_var <- apply_dummies(dummylist, newdata = x_train2)
head(a_new_var)

## What if you have two variables with the same name?
x_train3 <- x_train
colnames(x_train3) <- c("X1", "X2", "X3", "X3")
head(x_train3)
dummylist_same_var_name <- make_dummies(data = rbind(x_train3)) # will throw an error


## What if variables don't have names?
x_train3 <- x_train
colnames(x_train3) <- c("", "", "", "")
head(x_train3)
dummylist_no_var_names <- make_dummies(data = rbind(x_train3)) # will throw an error
