# Comparing original kernelshap with our implementation with that of Lundberg.



library(MASS)
library(xgboost)
library(shapr)
library(ggplot2)
library(data.table)

#### Installing shapper -- R wrapper for Lundbergs Python shap package using the reticulate package ####

# Python settings
library(reticulate)
#virtualenv_create("py3_6-virtualenv", python = "/usr/bin/python3.6") # Creating virtual environment with Python 3.6
#use_virtualenv("py3_6-virtualenv")


# Need to set up pip first, with a
library("shapper")
#install_shap(envname = "py3_6-virtualenv")

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


# Prepare the data for explanation
l <- prepare_kshap(
  Xtrain = x_train,
  Xtest = x_test
)

# Spedifying the phi_0, i.e. the expected prediction without any features
pred_zero <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero,
  empirical_settings = list(type = "independence",
       w_threshold = 1)
)

explanation.default <- compute_kshap(
  model = model,
  l = l,
  pred_zero = pred_zero,
)



# Printing the Shapley values for the test data
Kshap <- explanation$Kshap

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
                                             predict_function=p_function,nsamples = 1000)
  Kshap_original[i,1] <- out$`_yhat_mean_`[1] # same for all
  Kshap_original[i,1:ncol(x_test)+1] <- out$`_attribution_`
}

Kshap_original <- as.data.table(Kshap_original)
colnames(Kshap_original) <- colnames(Kshap)

##### Trying the SHAPforxgboost R package #####

devtools::install_github("liuyanguu/SHAPforxgboost")
library(SHAPforxgboost)
shap.values(xgb_model = model, X_train = dataX)


#

Kshap_original
Kshap

###############33

