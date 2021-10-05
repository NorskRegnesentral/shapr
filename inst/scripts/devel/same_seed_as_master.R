library(xgboost)
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
explainer <- shapr(x_train, model,n_combinations = 100)
p = mean(y_train)
gauss = explain(x_test, explainer, "gaussian", prediction_zero = p, n_samples = 10000)
emp =  explain(x_test, explainer, "empirical", prediction_zero = p, n_samples = 10000)
copula =  explain(x_test, explainer, "copula", prediction_zero = p, n_samples = 10000)
indep = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000)
comb = explain(x_test, explainer, c("gaussian", "gaussian", "empirical", "empirical"), prediction_zero = p, n_samples = 10000)
ctree = explain(x_test, explainer, "ctree", mincriterion = 0.95, prediction_zero = p, n_samples = 10000)
ctree2 = explain(x_test, explainer, "ctree", mincriterion = c(0.95, 0.95, 0.95, 0.95), prediction_zero = p, n_samples = 10000)


# results from master

res_master = readRDS("inst/scripts/devel/master_res.rds")

all.equal(comb$dt, res_master$comb$dt) #TRUE
all.equal(comb$p, res_master$comb$p) #TRUE
