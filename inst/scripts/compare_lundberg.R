library(MASS)
library(xgboost)
library(shapr)
library(data.table)

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Creating a larger test data set (300 observations) for more realistic function time calls.
# Modifying x_test to repeat the 6 test observations 50 times
x_test = rep(1,50) %x% x_test
colnames(x_test) <- colnames(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)

pred_test <- predict(model,x_test)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(predict(model,x_train))# adjustment from the standard mean(y_train) to comply with the shap implementation

time_R_start <- proc.time()
# Prepare the data for explanation
explainer <- shapr(x_train, model)

time_R_prepare <- proc.time()

#### TO be deleted ####
l <- prepare_kshap(
  Xtrain = x_train,
  Xtest = x_test
)
explanation_independence0 <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero,
  empirical_settings = list(type = "independence",
                            w_threshold = 1)
)
explanation_largesigma0 <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero,
  cond_approach = "empirical",
  empirical_settings = list(type ="fixed_sigma", fixed_sigma_vec = 10000, w_threshold = 1)
)

### END TO BE DELETED ####

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation_independence <- explain(x_test, explainer, approach = "empirical", type = "independence", prediction_zero = p0)

time_R_indep0 <- proc.time()

explanation_largesigma <- explain(x_test, explainer, approach = "empirical", type = "fixed_sigma",
                                  fixed_sigma_vec = 10000, w_threshold = 1, prediction_zero = p0)

time_R_largesigma0 <- proc.time()



time_R_indep <- time_R_indep0 - time_R_start
time_R_largesigma <- (time_R_largesigma0 - time_R_indep0) + (time_R_prepare- time_R_start)


# Printing the Shapley values for the test data
Kshap_indep <- explanation_independence$dt
Kshap_largesigma <- explanation_largesigma$dt

head(Kshap_indep)
#> Kshap_indep
#       none    lstat         rm         dis     indus
#1: 22,41355 7,116128  0,5203017 -1,91427784 3,1657530
#2: 22,41355 2,173011 -1,2201068 -0,47653736 0,3620256
#3: 22,41355 8,280909  3,7869719 -1,96968536 0,6037250
#4: 22,41355 8,384073  2,9590225 -2,19376523 1,8672685
#5: 22,41355 4,212031  3,8319436 -0,06695137 1,3392699
#6: 22,41355 3,295275 -1,2450126 -0,70618891 1,0924035

head(Kshap_largesigma)
#> Kshap_largesigma
#       none    lstat         rm        dis     indus
#1: 22,41355 7,116128  0,5203018 -1,9142779 3,1657530
#2: 22,41355 2,173011 -1,2201069 -0,4765373 0,3620255
#3: 22,41355 8,280910  3,7869718 -1,9696854 0,6037249
#4: 22,41355 8,384073  2,9590226 -2,1937652 1,8672685
#5: 22,41355 4,212031  3,8319435 -0,0669514 1,3392700
#6: 22,41355 3,295275 -1,2450126 -0,7061889 1,0924036


# Checking the difference between the methods
mean(abs(as.matrix(Kshap_indep)-as.matrix(Kshap_largesigma)))
#[1] 8.404487e-08  # Numerically identical


xgb.save(model=model,fname = "inst/compare_lundberg.xgb.obj") # Need to wait a bit after saving and then loading this in python

#### Running shap from Python ####

# Python settings
library(reticulate)
#virtualenv_create("py3_6-virtualenv", python = "/usr/bin/python3.6") # Creating virtual environment with Python 3.6
use_virtualenv("py3_6-virtualenv")
#py_install("xgboost",envname = "py3_6-virtualenv")
#py_install("shap",envname = "py3_6-virtualenv")



reticulate::repl_python()
#### Python code ####
import xgboost as xgb
import shap
import numpy as np
import pandas as pd
import time

model = xgb.Booster()  # init model
model.load_model("inst/compare_lundberg.xgb.obj")

## kernel shap sends data as numpy array which has no column names, so we fix it
def xgb_predict(data_asarray):
  data_asDmatrix =  xgb.DMatrix(data_asarray)
  return model.predict(data_asDmatrix)

py_pred_test = xgb_predict(r.x_test) # Test predictions in python

sum((py_pred_test-r.pred_test)**2) # checking equality with r predictions

#### Applying kernelshap

time_py_start = time.perf_counter()

shap_kernel_explainer = shap.KernelExplainer(xgb_predict, r.x_train)
Kshap_shap0 = shap_kernel_explainer.shap_values(r.x_test,nsamples = int(100000),l1_reg=0)

time_py_end = time.perf_counter()

time_py = time_py_end-time_py_start

getattr(shap_kernel_explainer,'expected_value') # This is phi0, not used at all below

Kshap_shap = pd.DataFrame(Kshap_shap0,columns = r.x_var)

Kshap_shap.insert(0,"none",getattr(shap_kernel_explainer,'expected_value'),True) # Adding the none column


exit
#### Exit python code ####

head(Kshap_indep)
#> Kshap_indep
#       none    lstat         rm         dis     indus
#1: 22,41355 7,116128  0,5203017 -1,91427784 3,1657530
#2: 22,41355 2,173011 -1,2201068 -0,47653736 0,3620256
#3: 22,41355 8,280909  3,7869719 -1,96968536 0,6037250
#4: 22,41355 8,384073  2,9590225 -2,19376523 1,8672685
#5: 22,41355 4,212031  3,8319436 -0,06695137 1,3392699
#6: 22,41355 3,295275 -1,2450126 -0,70618891 1,0924035

head(py$Kshap_shap)
#> py$Kshap_shap
#      none    lstat         rm         dis     indus
#1 22,41355 7,116128  0,5203018 -1,91427784 3,1657530
#2 22,41355 2,173011 -1,2201069 -0,47653727 0,3620255
#3 22,41355 8,280910  3,7869719 -1,96968537 0,6037250
#4 22,41355 8,384073  2,9590226 -2,19376508 1,8672686
#5 22,41355 4,212031  3,8319435 -0,06695135 1,3392701
#6 22,41355 3,295275 -1,2450126 -0,70618891 1,0924036


# Checking difference between our R implementtaion and the shap implementation i Python
mean(abs(as.matrix(Kshap_indep)-as.matrix(py$Kshap_shap)))
#[1] 1,300368e-07 # Numerically identical

# Checking the running time of the different methods
time_R_indep[3]
time_R_largesigma[3]
py$time_py
#> time_R_indep[3]
#elapsed
#9,908
#> time_R_largesigma[3]
#elapsed
#9,768
#> py$time_py
#[1] 10,75703

# Our R implementation is about 1 second = 10% faster.
# Might be some overhead by calling Python from R, but I don't think it's that much.
