#### Prediction with categorical input ####

rm(list=ls())

library(xgboost)
library(shapr)
library(data.table)
library(party)

data("Boston", package = "MASS")


x_var <- c("lstat", "rm", "dis", "indus", "rad")
 x_var <- c("lstat","black","indus","ptratio","tax","rad","age","dis","rm","nox")

y_var <- "medv"

Boston$rad = as.factor(Boston$rad)
Boston$chas = as.factor(Boston$chas)


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

library(microbenchmark)

timing <- microbenchmark(explanation <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE),
  explanation.mc2 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores = 2),
  explanation.mc4 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores = 4),
  explanation.mc8 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores = 8),
  times = 10)


timing2 <- microbenchmark(explanation <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE),
  explanation.mc1_2 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores_simulateAllTrees = 2,
    mc.cores_sample_ctree = 1),
  explanation.mc4 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores_simulateAllTrees = 4,
    mc.cores_sample_ctree = 1),
  explanation.mc8 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores_simulateAllTrees = 8,
    mc.cores_sample_ctree = 1),
  times = 10)

timing3 <- microbenchmark(explanation <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE),
  explanation.mc1_2 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores_simulateAllTrees = 1,
    mc.cores_sample_ctree = 2),
  explanation.mc4 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores_simulateAllTrees = 1,
    mc.cores_sample_ctree = 4),
  explanation.mc8 <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE,
    mc.cores_simulateAllTrees = 1,
    mc.cores_sample_ctree = 8),
  times = 10)



all.equal(explanation,explanation.mc2)
all.equal(explanation,explanation.mc4)
all.equal(explanation,explanation.mc8)

timing # Almost identical for dimension 6. Slower for dimension smaller than 6
timing2 # Almost identical for dimension 6. Slower for dimension smaller than 6
timing3 # Almost identical for dimension 6. Slower for dimension smaller than 6



# Printing the Shapley values for the test data
print(explanation$dt)

# Finally we plot the resulting explanations
plot(explanation)


