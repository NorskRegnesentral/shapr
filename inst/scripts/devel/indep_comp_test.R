library(xgboost)
library(shapr)
library(MASS)
library(data.table)

data("Boston")

x_var <- c("crim","zn","indus","chas"   ,"nox"  ,  "rm" ,  "age"  ,  "dis" ,"rad" ,"tax" ,"ptratio",  "black", "lstat")[1:6]
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Creating a larger test data set (600 observations) for more realistic function time calls.
# Modifying x_test to repeat the 6 test observations 50 times
x_test = x_test[rep(1,10),]#rep(1,100) %x% x_test
colnames(x_test) <- colnames(x_train)

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
aa <- list()
i <- 1
for (i in 1:100){
  explanation_old <- explain(
    x_test,
    approach = "empirical",
    type = "independence",
    explainer = explainer,
    prediction_zero = p, seed=111+i,n_samples = 100
  )
  aa[[i]] <- copy(explanation_old$dt)
}


colMeans(rbindlist(aa,idcol = "id"))[-1]
# Result with shapr pre august 2021
# (https://github.com/NorskRegnesentral/shapr/commit/be0d8820d03a40ce2a6b97119951e9e53f565bf3 and earlier)

# Result with shapr post august 2021
#none       crim         zn      indus       chas        nox         rm
#22.4459999  0.5635402  0.2107961 -0.3621242 -0.2246083 -0.3147309  0.9694476

explanation_full <- explain(
  x_test[rep(1,5),],
  approach = "empirical",
  type = "independence",
  explainer = explainer,
  prediction_zero = p, seed=111+i
)
explanation_full$dt
# Using full data set into Monte Carlo integration (all give the same result, much closer to new indep version)
#none      crim        zn      indus       chas        nox        rm
#1: 22.446 0.5669853 0.2103577 -0.3720835 -0.2213791 -0.3109162 0.9693563
#2: 22.446 0.5669850 0.2103577 -0.3720835 -0.2213790 -0.3109161 0.9693564
#3: 22.446 0.5669854 0.2103576 -0.3720834 -0.2213791 -0.3109162 0.9693563
#4: 22.446 0.5669850 0.2103576 -0.3720834 -0.2213787 -0.3109163 0.9693563
#5: 22.446 0.5669851 0.2103578 -0.3720834 -0.2213793 -0.3109162 0.9693564


