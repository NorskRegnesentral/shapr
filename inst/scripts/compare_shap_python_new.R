library(MASS)
library(xgboost)
library(shapr)
library(data.table)

# Python settings
# Using the virtual environment here "../../Python/.venv/bin/python", as set by
#Sys.setenv(RETICULATE_PYTHON = "../../Python/.venv/bin/python") in the .Rprofile
library(reticulate)

# Install some packages
#py_install("xgboost")
#py_install("shap")
#py_install("pandas")

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Creating a larger test data set (600 observations) for more realistic function time calls.
# Modifying x_test to repeat the 6 test observations 100 times
x_test = rep(1,100) %x% x_test
colnames(x_test) <- colnames(x_train)

# Reading the R format version of the xgboost model to avoid crash reading same xgboost model in R and Python
model <- readRDS(system.file("model_objects", "xgboost_model_object.rds", package = "shapr"))

pred_test <- predict(model,x_test)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(predict(model,x_train))# adjustment from the standard mean(y_train) to comply with the shap implementation

time_R_start <- proc.time()

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation_independence <- explain(model = model,x_explain = x_test,x_train=x_train,
                                    approach = "independence", prediction_zero = p0,n_batches = 1)

time_R_indep0 <- proc.time()


explanation_largesigma <- explain(model = model,x_explain = x_test,x_train=x_train,
                                  approach = "empirical",empirical.type="fixed_sigma",empirical.fixed_sigma=10000,empirical.eta=1,
                                  prediction_zero = p0,n_batches=1)


time_R_largesigma0 <- proc.time()

(time_R_indep <- time_R_indep0 - time_R_start)
(time_R_largesigma <- time_R_largesigma0 - time_R_indep0)

# Printing the Shapley values for the test data
Kshap_indep <- explanation_independence$shapley_values
Kshap_largesigma <- explanation_largesigma$shapley_values

Kshap_indep
Kshap_largesigma

mean(abs(as.matrix(Kshap_indep)-as.matrix(Kshap_largesigma)))

reticulate::py_run_file(system.file("scripts", "shap_python_script.py", package = "shapr"))
