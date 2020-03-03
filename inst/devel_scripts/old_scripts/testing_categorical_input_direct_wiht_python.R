library(MASS)
library(xgboost)
library(shapr)
library(data.table)

# Python settings
library(reticulate)
#virtualenv_create("py3_6-virtualenv", python = "/usr/bin/python3.6") # Creating virtual environment with Python 3.6
use_virtualenv("py3_6-virtualenv")
#py_install("xgboost",envname = "py3_6-virtualenv")
#py_install("shap",envname = "py3_6-virtualenv")

data("Boston")

x_var <- c("lstat", "rm")
y_var <- "medv"



x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

set.seed(123)
x_train <- as.data.frame(cbind(x_train,sample(0:2,nrow(x_train),replace=T),sample(0:2,nrow(x_train),replace=T)))
x_test <- as.data.frame(cbind(x_test,sample(0:2,nrow(x_test),replace=T),sample(0:2,nrow(x_test),replace=T)))

colnames(x_train)[3:4] = paste0("cat_",1:2)
colnames(x_test) <- colnames(x_train)

x_train$cat_1 <- as.factor(x_train$cat_1)
x_train$cat_2 <- as.factor(x_train$cat_2)

x_test$cat_1 <- as.factor(x_test$cat_1)
x_test$cat_2 <- as.factor(x_test$cat_2)

library(caret)
dummyfunc <- caret::dummyVars(" ~ .-1", data = rbind(x_train, x_test))
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 20
)

saveRDS(model,file = "inst/model_objects/xgboost_model_object_cat.rds")

xgb.save(model=model,fname = "inst/model_objects/xgboost_model_object_raw_cat")

library(reticulate)

repl_python()
###########


#pred_test <- predict(model,x_test)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(predict(model,x_train))# adjustment from the standard mean(y_train) to comply with the shap implementation

time_R_start <- proc.time()
# Prepare the data for explanation
explainer <- shapr(x_train, model)

time_R_prepare <- proc.time()

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



#### Running shap from Python ####
reticulate::py_run_file(system.file("scripts", "shap_python_script.py", package = "shapr"))
# Writes Python objects to the list py #

# Checking that the predictions are identical
sum((pred_test-py$py_pred_test)^2)

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
#7,417
#> time_R_largesigma[3]
#elapsed
#6,271
#> py$time_py
#[1] 21,23536

# Our R implementation is about 3 times faster than the the shap package on this task.
# Might be some overhead by calling Python from R, but it shouldn't be even close to that much.






