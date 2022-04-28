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


# No specification (sequential)
start <- proc.time()
explanation0 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time0 <- stop-start


# Sequential
start <- proc.time()
future::plan("sequential")
explanation1 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time1 <- stop-start

# Try to set multicore (in Rstudio this is disabled so falls back to sequential)
start <- proc.time()
future::plan("multicore",workers=5)  ## defaults to availableCores() workers
explanation2 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time2 <- stop-start

# Multisession with 2 workers
start <- proc.time()
future::plan("multisession",workers = 2)  ## defaults to availableCores() workers
explanation3 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time3 <- stop-start

# Multisession with 5 workers
start <- proc.time()
future::plan("multisession",workers=5)  ## defaults to availableCores() workers
explanation4 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time4 <- stop-start

# Multisession with 10 workers
start <- proc.time()
future::plan("multisession",workers=10)  ## defaults to availableCores() workers
explanation5 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time5 <- stop-start

# Multisession with 20 workers
start <- proc.time()
future::plan("multisession",workers=20)
explanation6 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
time6 <- stop-start

# Trying to set up a cluster and run it there
start <- proc.time()
cl <- parallel::makeCluster(c(rep("hpc01",5),rep("hpc02",5),rep("hpc03",6)),
                            rscript = "/nr/prog/AppServerDefaults/Ubuntu_18.04_x86_64/bin/Rscript")
plan(cluster, workers = cl)
#plan(remote, workers = cl)
explanation7 <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,n_batches = 32
)
stop <- proc.time()
parallel::stopCluster(cl)
time7 <- stop-start





# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
head(explanation0$dt,2)
head(explanation1$dt,2)
head(explanation2$dt,2)
head(explanation3$dt,2)
head(explanation4$dt,2)
head(explanation5$dt,2)
head(explanation6$dt,2)
head(explanation7$dt,2)

cbind(time0,time1,time2,time3,time4,time5,time6,time7)
