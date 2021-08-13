library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("crim","zn","indus","chas"   ,"nox"  ,  "rm" ,  "age"  ,  "dis" ,"rad" ,"tax" ,"ptratio",  "black", "lstat")[1:6]
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

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
aa <- list()
t_old <- proc.time()
for (i in 1:1000){
  explanation_old <- explain(
    x_test[rep(1,5),],
    approach = "empirical",
    type = "independence",
    explainer = explainer,
    prediction_zero = p, seed=111+i,n_samples = 100
  )
  aa[[i]] <- copy(explanation_old$dt)
}
print(proc.time()-t_old)
#user  system elapsed
#57.348  39.521  37.355

bb <- list()
t_new <- proc.time()
for (i in 1:1000){
  explanation_new <- explain(
    x_test[rep(1,5),],
    approach = "independence",
    explainer = explainer,
    prediction_zero = p,seed = i+23123,n_samples = 100
  )
  bb[[i]] <- copy(explanation_new$dt)
}
print(proc.time()-t_new)
#user  system elapsed
#46.015   7.567  23.616

colMeans(rbindlist(aa,idcol = "id"))
colMeans(rbindlist(bb,idcol = "id"))

explanation_full_old <- explain(
  x_test[rep(1,5),],
  approach = "empirical",
  type = "independence",
  explainer = explainer,
  prediction_zero = p, seed=111+i
)


explanation_full_new <- explain(
  x_test[rep(1,5),],
  approach = "independence",
  explainer = explainer,
  prediction_zero = p,seed = i+23123
)
explanation_full_new$dt
explanation_full_old$dt


# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)

# Finally we plot the resulting explanations
plot(explanation)
