library(progressr)
library(future.apply)
library(xgboost)
#library(shapr) # use devtools::load_all() instead when experimenting
library(data.table)

data("airquality")
airquality <- airquality[complete.cases(airquality), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_test <- 1:20
x_train <- as.matrix(airquality[-ind_x_test, x_var])
y_train <- airquality[-ind_x_test, y_var]
x_test <- as.matrix(airquality[ind_x_test, x_var])


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
p <- mean(y_train)

# By default using rstudio progressbar in RStudio, progress is that is installed, otherwise txtprogressbar
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 10,n_samples=10^5)
#### THIS DOES NOT WORK AS INTENDED -- txtprogressbar is still used when none are specified -- DONT UNDERSTAND WHY



# Can also set them manually
progressr::handlers("rstudio")
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 10,n_samples=10^5)

progressr::handlers("progress")
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 10,n_samples=10^5)

# you can edit the symbol used to draw completed progress in the progress bar (as well as other features) with handler_progress()
handlers(handler_progress(complete = "#"))
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 10,n_samples = 10^5)

# This also works for parallelized setups
plan(multisession,workers = 3)
x <- explain(x_train, x_test, model, approach="gaussian", prediction_zero=p, n_batches = 10,n_samples=10^5)

