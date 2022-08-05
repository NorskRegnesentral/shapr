
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
library(shapr)
explainer <- shapr(x_train, model,n_combinations = 100)
p = mean(y_train)
gauss = explain(x_test, explainer, "gaussian", prediction_zero = p, n_samples = 10000)
emp =  explain(x_test, explainer, "empirical", prediction_zero = p, n_samples = 10000)
copula =  explain(x_test, explainer, "copula", prediction_zero = p, n_samples = 10000)
indep = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000)
comb = explain(x_test, explainer, c("gaussian", "gaussian", "empirical", "empirical"), prediction_zero = p, n_samples = 10000)
ctree = explain(x_test, explainer, "ctree", mincriterion = 0.95, prediction_zero = p, n_samples = 10000)
ctree2 = explain(x_test, explainer, "ctree", mincriterion = c(0.95, 0.95, 0.95, 0.95), prediction_zero = p, n_samples = 10000)
#saveRDS(list(gauss = gauss, empirical = emp, copula = copula, indep = indep, comb = comb, ctree = ctree, ctree_comb = ctree2), file = "inst/scripts/devel/master_res2.rds")
# saveRDS(list(ctree = ctree, ctree_comb = ctree2), file = "inst/scripts/devel/master_res_ctree.rds")


detach("package:shapr", unload = TRUE)
devtools::load_all()
nobs = 6
x_test <- as.matrix(Boston[1:nobs, x_var])
explainer <- shapr(x_train, model,n_combinations = 100)
p = mean(y_train)
gauss = explain(x_test, explainer, "gaussian", prediction_zero = p, n_samples = 10000, n_batches = 1)
emp =  explain(x_test, explainer, "empirical", prediction_zero = p, n_samples = 10000, n_batches = 1)
copula =  explain(x_test, explainer, "copula", prediction_zero = p, n_samples = 10000, n_batches = 1)
indep = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000, n_batches = 1)
comb = explain(x_test, explainer, c("gaussian", "gaussian", "empirical", "empirical"), prediction_zero = p, n_samples = 10000, n_batches = 1)
ctree = explain(x_test, explainer, "ctree", mincriterion = 0.95, prediction_zero = p, n_samples = 10000, n_batches = 1)
ctree2 = explain(x_test, explainer, "ctree", mincriterion = c(0.95, 0.95, 0.95, 0.95), prediction_zero = p, n_samples = 10000, n_batches = 1)

res = readRDS("inst/scripts/devel/master_res2.rds")

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

#### MJ stuff here:

explain.independence2 <- function(x, explainer, approach, prediction_zero,
                                  n_samples = 1e3, n_batches = 1, seed = 1, only_return_contrib_dt = FALSE, ...) {


  if (!is.null(seed)) set.seed(seed)

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_specs)$x_dt)
  explainer$approach <- approach
  explainer$n_samples <- n_samples

  r <- prepare_and_predict(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...)
}


prepare_data.independence2 <- function(x, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  if (is.null(index_features)) {
    index_features <- x$X[, .I]
  }

  S <- x$S[index_features, ]
  x_train <- as.matrix(x$x_train)
  n_train <- nrow(x_train)
  n_samples <- min(x$n_samples, n_train)

  index_s <- rep(seq(nrow(S)), each = n_samples)
  w <- 1 / x$n_samples

  n_col <- nrow(x$x_test)

  dt_l <- list()
  for (i in seq(n_col)) {
    x_test <- x$x_test[i, , drop = FALSE]

    # sampling index_xtrain
    index_xtrain <- c(replicate(nrow(S), sample(x = seq(n_train), size = n_samples, replace = F)))

    # Generate data used for prediction
    dt_p <- observation_impute_cpp(
      index_xtrain = index_xtrain,
      index_s = index_s,
      xtrain = x_train,
      xtest = x_test,
      S = S
    )

    # Add keys
    dt_l[[i]] <- data.table::as.data.table(dt_p)
    data.table::setnames(dt_l[[i]], colnames(x_train))
    dt_l[[i]][, id_combination := index_s]
    dt_l[[i]][, w := w] # IS THIS NECESSARY?
    dt_l[[i]][, id := i]
  }


  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}




# Using independence with n_samples > nrow(x_train) such that no sampling is performed

indep1 =  explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000, n_batches = 1)
indep2 =  explain(x_test, explainer, "independence2", prediction_zero = p, n_samples = 10000, n_batches = 1)

all.equal(indep1,indep2) # TRUE

indep1_batch_2 = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000, n_batches = 2)

all.equal(indep1,indep1_batch_2) # TRUE

indep1_batch_5 = explain(x_test, explainer, "independence", prediction_zero = p, n_samples = 10000, n_batches = 5)

all.equal(indep1,indep1_batch_5) # TRUE

comb_indep_1_batch_1 = explain(x_test, explainer, c("independence", "independence", "independence", "independence"), prediction_zero = p, n_samples = 10000, n_batches = 1)

all.equal(indep1,comb_indep_1_batch_1) # TRUE

comb_indep_1_batch_2 = explain(x_test, explainer, c("independence", "independence", "independence", "independence"), prediction_zero = p, n_samples = 10000, n_batches = 2)

all.equal(indep1,comb_indep_1_batch_2) # TRUE

comb_indep_1_2_batch_1 = explain(x_test, explainer, c("independence", "independence", "independence2", "independence2"), prediction_zero = p, n_samples = 10000, n_batches = 1)

all.equal(indep1,comb_indep_1_2_batch_1) #TRUE

comb_indep_1_2_batch_2 = explain(x_test, explainer, c("independence", "independence", "independence2", "independence2"), prediction_zero = p, n_samples = 10000, n_batches = 2)

all.equal(indep1,comb_indep_1_2_batch_2) #TRUE

comb_indep_1_2_batch_5 = explain(x_test, explainer, c("independence", "independence", "independence2", "independence2"), prediction_zero = p, n_samples = 10000, n_batches = 5)

all.equal(indep1,comb_indep_1_2_batch_5) #TRUE

