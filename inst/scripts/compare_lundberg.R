# Comparing original kernelshap with our implementation with that of Lundberg using the readme example


library(MASS)
library(xgboost)
library(shapr)
library(ggplot2)
library(data.table)


data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))
# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)

pred_test <- predict(model,x_test)

# Prepare the data for explanation
l <- prepare_kshap(
  Xtrain = x_train,
  Xtest = x_test
)

# Spedifying the phi_0, i.e. the expected prediction without any features
pred_zero <- mean(predict(model,x_train))# adjustment from the standard mean(y_train) to comply with the shap implementatin

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation.independence <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero,
  empirical_settings = list(type = "independence",
       w_threshold = 1)
)

explanation.largesigma <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero,
  cond_approach = "empirical",
  empirical_settings = list(type ="fixed_sigma", fixed_sigma_vec = 10000, w_threshold = 1)
)


# Printing the Shapley values for the test data
Kshap_indep <- explanation.independence$Kshap
Kshap_largesigma <- explanation.largesigma$Kshap

# Checking the difference between the methods
sum((as.matrix(Kshap_indep)-as.matrix(Kshap_largesigma))^2)
#[1] 1,264819e-13 # Numerically identical


#xgb.save(model=model,fname = "inst/compare_lundberg.xgb.obj") # Need to wait a bit after saving and then loading this in python



################# Running shap from Python #####

# Python settings
library(reticulate)
#virtualenv_create("py3_6-virtualenv", python = "/usr/bin/python3.6") # Creating virtual environment with Python 3.6
use_virtualenv("py3_6-virtualenv")
#py_install("xgboost",envname = "py3_6-virtualenv")
#py_install("shap",envname = "py3_6-virtualenv")



repl_python()
#### Python code
import xgboost as xgb
import shap
import numpy as np
import pandas as pd

model = xgb.Booster()  # init model
model.load_model("inst/compare_lundberg.xgb.obj")

## kernel shap sends data as numpy array which has no column names, so we fix it
def xgb_predict(data_asarray):
  data_asDmatrix =  xgb.DMatrix(data_asarray)
  return model.predict(data_asDmatrix)

py_pred_test = xgb_predict(r.x_test) # Test predictions in python

sum((py_pred_test-r.pred_test)**2) # checking equality with r predictions

#### Applying kernelshap

shap_kernel_explainer = shap.KernelExplainer(xgb_predict, r.x_train)

getattr(shap_kernel_explainer,'expected_value') # This is phi0, not used at all below

Kshap_shap0 = shap_kernel_explainer.shap_values(r.x_test,nsamples = int(100000),l1_reg=0)

Kshap_shap = pd.DataFrame(Kshap_shap0,columns = r.x_var)

Kshap_shap.insert(0,"none",getattr(shap_kernel_explainer,'expected_value'),True) # Adding the none column

Kshap_shap


exit
# Exit python code


# Checking difference between our R implementtaion and the shap implementation i Python
sum((as.matrix(Kshap_indep)-as.matrix(py$Kshap_shap))^2)
#[1] 4,742449e-13 # Numerically identical

