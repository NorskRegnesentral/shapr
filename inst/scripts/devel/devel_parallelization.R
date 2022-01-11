library(xgboost)
library(shapr)
library(future)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus","rad","tax","ptratio","black","zn","crim")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:20, x_var])

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


#plan(cluster, workers = c("localhost","samba8.ad.nr.no"))
#cl <- parallel::makeCluster(c("localhost","samba8.ad.nr.no"))

start <- proc.time()
future::plan("multicore",workers=5)  ## defaults to availableCores() workers
explanation <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
stop-start

future::plan("multisession",workers = 10)  ## defaults to availableCores() workers

start <- proc.time()
future::plan("sequential")  ## defaults to availableCores() workers
explanation <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
stop-start

start <- proc.time()
future::plan("multisession",workers=2)  ## defaults to availableCores() workers
explanation <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
stop-start
# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)

# Finally we plot the resulting explanations
plot(explanation)
