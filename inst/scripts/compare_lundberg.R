# Comparing original kernelshap with our implementation with that of Lundberg.



library(MASS)
library(xgboost)
library(shapr)
library(ggplot2)
library(data.table)

#### Installing shapper -- R wrapper for Lundbergs Python shap package using the reticulate package ####


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
r.Kshap_indep
r.Kshap_largesigma


exit
# Exit python code






#### Applying shapper ####
x_test_df <- as.data.frame(x_test)
x_train_df <- as.data.frame(x_train)


p_function <- function(model, data) predict(model, newdata = as.matrix(data))
p_function(model,x_test_df)

Kshap_original <- matrix(NA,ncol=ncol(x_test)+1,nrow=nrow(x_test))

for(i in 1:nrow(x_test)){

  out = shapper::individual_variable_effect(x = model,
                                             data = x_train_df,
                                             new_observation = x_test_df[i,],
                                             predict_function=p_function,nsamples = 10000)
  Kshap_original[i,1] <- out$`_yhat_mean_`[1] # same for all
  Kshap_original[i,1:ncol(x_test)+1] <- out$`_attribution_`
}

Kshap_original <- as.data.table(Kshap_original)
colnames(Kshap_original) <- colnames(Kshap)


Kshap_original
Kshap

###############33

