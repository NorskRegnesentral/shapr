library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("crim", "zn", "indus","chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat")[1:6]
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

x_test <- x_test[rep(1,1000),]

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
t_old <- proc.time()
explanation_old <- explain(
  x_test,
  approach = "empirical",
  type = "independence",
  explainer = explainer,
  prediction_zero = p, seed=111,n_samples = 100
)
print(proc.time()-t_old)
#user  system elapsed
#64.228   2.829  16.455

t_new <- proc.time()
explanation_new <- explain(
  x_test,
  approach = "independence",
  explainer = explainer,
  prediction_zero = p,seed = 111,n_samples = 100
  )
print(proc.time()-t_new)
#user  system elapsed
#10.376   0.731   4.907

colMeans(explanation_old$dt)
colMeans(explanation_new$dt)
#> colMeans(explanation_old$dt)
#none       crim         zn      indus       chas        nox         rm
#22.4459999  0.5606242  0.2137357 -0.3738064 -0.2214088 -0.3129846  0.9761603
#> colMeans(explanation_new$dt)
#none       crim         zn      indus       chas        nox         rm
#22.4459999  0.5638315  0.2087042 -0.3697038 -0.2155639 -0.3173748  0.9724273

t_old <- proc.time()
explanation_full_old <- explain(
  x_test,
  approach = "empirical",
  type = "independence",
  explainer = explainer,
  prediction_zero = p, seed=111
)
print(proc.time()-t_old)
#user  system elapsed
#96.064   6.679  29.782

t_new <- proc.time()
explanation_full_new <- explain(
  x_test,
  approach = "independence",
  explainer = explainer,
  prediction_zero = p,seed = 111
)
print(proc.time()-t_new)
#user  system elapsed
#40.363   5.978  16.982

explanation_full_old$dt
explanation_full_new$dt

#> explanation_full_old$dt
#none      crim        zn      indus       chas        nox        rm
#1: 22.446 0.5669854 0.2103575 -0.3720833 -0.2213789 -0.3109162 0.9693561
#2: 22.446 0.5669846 0.2103578 -0.3720834 -0.2213790 -0.3109160 0.9693565
#3: 22.446 0.5669852 0.2103576 -0.3720835 -0.2213789 -0.3109162 0.9693562
#4: 22.446 0.5669855 0.2103575 -0.3720834 -0.2213790 -0.3109163 0.9693562
#5: 22.446 0.5669851 0.2103576 -0.3720833 -0.2213794 -0.3109161 0.9693566
#---
#  996: 22.446 0.5669856 0.2103575 -0.3720833 -0.2213791 -0.3109163 0.9693562
#997: 22.446 0.5669851 0.2103575 -0.3720833 -0.2213790 -0.3109162 0.9693563
#998: 22.446 0.5669854 0.2103576 -0.3720834 -0.2213790 -0.3109163 0.9693562
#999: 22.446 0.5669853 0.2103575 -0.3720832 -0.2213791 -0.3109162 0.9693562
#1000: 22.446 0.5669846 0.2103577 -0.3720833 -0.2213790 -0.3109161 0.9693565
#> explanation_full_new$dt
#none      crim        zn      indus      chas        nox        rm
#1: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#2: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#3: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#4: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#5: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#---
#  996: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#997: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#998: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#999: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563
#1000: 22.446 0.5669853 0.2103576 -0.3720834 -0.221379 -0.3109162 0.9693563

