#### Prediction with categorical input ####

rm(list=ls())

library(xgboost)
library(shapr)
library(data.table)
library(party)

data("Boston", package = "MASS")


x_var <- c("lstat", "rm", "dis", "indus", "rad")
# x_var <- c("lstat", "rm", "dis", "indus")

y_var <- "medv"

Boston$rad = as.factor(Boston$rad)

x_train <- Boston[-(1:6), x_var]
y_train <- Boston[-(1:6), y_var]
x_test <- Boston[1:6, x_var]


### SPECIAL STUFF  STARTS ###
library(caret)
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test))
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)
### SPECIAL STUFF ENDS ###


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

### SPEXIAL STUFF STARTS ###
model$dummyfunc <- dummyfunc
### SPECIAL STUFF ENDS ###

explainer <- shapr(x_train, model)

p <- mean(y_train)

explanation <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)


# Printing the Shapley values for the test data
print(explanation$dt)

# Finally we plot the resulting explanations
plot(explanation)


