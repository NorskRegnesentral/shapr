
library(xgboost)
devtools::load_all()
#library(shapr)
library(data.table)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")#,"nox","age","tax","ptratio")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# THIS IS GENERATED FROM MASTER BRANCH
# Prepare the data for explanation
# explainer <- shapr(x_train, model,n_combinations = 100)
# p = mean(y_train)
# gauss = explain(x_test, explainer, "gaussian", prediction_zero = p, n_samples = 10000)
# emp =  explain(x_test, explainer, "empirical", prediction_zero = p, n_samples = 10000)
# copula =  explain(x_test, explainer, "copula", prediction_zero = p, n_samples = 10000)
# indep = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000)
# comb = explain(x_test, explainer, c("gaussian", "gaussian", "empirical", "empirical"), prediction_zero = p, n_samples = 10000)
#saveRDS(list(gauss = gauss, empirical = emp, copula = copula, indep = indep, comb = comb), file = "master_res.rds")
nobs = 6
x_test <- as.matrix(Boston[1:nobs, x_var])

explainer <- shapr(x_train, model,n_combinations = 100)
p = mean(y_train)
gauss = explain(x_test, explainer, "gaussian", prediction_zero = p, n_samples = 10000, n_batches = 1)
emp =  explain(x_test, explainer, "empirical", prediction_zero = p, n_samples = 10000, n_batches = 1)
copula =  explain(x_test, explainer, "copula", prediction_zero = p, n_samples = 10000, n_batches = 1)
indep = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000, n_batches = 1)
comb = explain(x_test, explainer, c("gaussian", "empirical", "empirical", "empirical"), prediction_zero = p, n_samples = 10000, n_batches = 1)
ctree = explain(x_test, explainer, "ctree", prediction_zero = p, n_samples = 10000, n_batches = 3)

res = readRDS("inst/scripts/devel/master_res.rds")

# Compare res
all.equal(res$gauss$dt, gauss$dt) # TRUE
all.equal(res$empirical$dt, emp$dt) # TRUE

res$comb$dt
comb$dt

# With batches
gauss_b = explain(x_test, explainer, "gaussian", prediction_zero = p, n_samples = 10000, n_batches = 3)
emp_b =  explain(x_test, explainer, "empirical", prediction_zero = p, n_samples = 10000, n_batches = 3)

gauss_b$dt
res$gauss$dt

emp_b$dt
res$empirical$dt
