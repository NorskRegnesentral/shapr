library(progressr)
library(future.apply)
library(xgboost)
library(shapr)
library(data.table)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus", "age", "ptratio")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-15, x_var])
y_train <- Boston[-1:-15, y_var]
x_test <- as.matrix(Boston[1:100, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
p <- mean(y_train)

plan(multisession, workers=3)

# when we simply call explain(), no progress bar is shown
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 4)

# the handler specifies what kind of progress bar is shown
# Wrapping explain() in with_progress() gives a progress bar when calling explain()
handlers("txtprogressbar")
x <- with_progress(
  explain(x_train, x_test, model, approach="empirical", prediction_zero=p, n_batches = 5)
  )

# with global=TRUE the progress bar is displayed whenever the explain-function is called, and there is no need to use with_progress()
handlers(global = TRUE)
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 4)

# there are different options for what kind of progress bar should be displayed
handlers("txtprogressbar") #this is the default
x <- explain(x_train, x_test, model, approach="independence", prediction_zero=p, n_batches = 4)

handlers("progress")
x <- explain(x_train, x_test, model, approach="independence", prediction_zero=p, n_batches = 4)

# you can edit the symbol used to draw completed progress in the progress bar (as well as other features) with handler_progress()
handlers(handler_progress(complete = "#"))
x <- explain(x_train, x_test, model, approach="copula", prediction_zero=p, n_batches = 4)

plan("sequential")

handlers("progress")
x <- explain(x_train, x_test, model, approach=c(rep("ctree",4),"independence","independence"), prediction_zero=p, n_batches = 4)



