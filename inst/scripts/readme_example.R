library(MASS)
library(xgboost)
library(shapr)

data("Boston")

x_var <-  c("lstat","rm","dis","indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-(1:10),x_var])
y_train <- Boston[-(1:10),y_var]
x_test <- as.matrix(Boston[1:10,x_var])

# Just looking at the dependence between the features

 cor(x_train)
 #> cor(x_train)
 # lstat         rm        dis      indus
 # lstat  1.0000000 -0.6106362 -0.5075796  0.6073291
 # rm    -0.6106362  1.0000000  0.2051866 -0.3897134
 # dis   -0.5075796  0.2051866  1.0000000 -0.7059103
 # indus  0.6073291 -0.3897134 -0.7059103  1.0000000


# Fitting a basic xgboost model to the training data
model <- xgboost(data = x_train,
                 label = y_train,
                 nround=20)


# Prepare the data for explanation
l <- prepare_kshap(Xtrain = x_train,
                   Xtest = x_test)

# Spedifying the phi_0, i.e. the expected prediction without any features
pred_zero <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation = compute_kshap(model = model,
                            l = l,
                            pred_zero=pred_zero)

# Printing the Shapley values for the test data
explanation$Kshap

#          [,1]      [,2]       [,3]        [,4]       [,5]
# [1,] 22.45484 5.3814963 -0.4823530  1.56978008  4.9216356
# [2,] 22.45484 0.3344492 -0.8322893  0.97687526  0.3750935
# [3,] 22.45484 6.1609843  4.8894905  0.53138409 -2.0826502
# [4,] 22.45484 8.9163071  0.3154509 -0.18616672  2.1797222
# [5,] 22.45484 0.7486236  6.2298609  0.24828116  2.7369589
# [6,] 22.45484 2.5868833 -3.8420937 -0.02532364  3.1038779
