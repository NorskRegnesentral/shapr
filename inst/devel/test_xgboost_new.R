
# Uncomment the following line to test with the OLD xgboost version (1.7.7.1)
# .libPaths(c("/home/jullum/R/x86_64-pc-linux-gnu-library/4.3_old_xgboost", .libPaths()))

#library(shapr)
devtools::load_all()
library(xgboost)
library(data.table)

message("Using xgboost version: ", packageVersion("xgboost"))

# Helper to handle xgboost version differences
safe_xgboost <- function(data, label, verbose = 0, ...) {
  if (packageVersion("xgboost") >= "2.0") {
    xgboost::xgboost(x = data, y = label, verbosity = verbose, ...)
  } else {
    xgboost::xgboost(data = data, label = label, verbose = verbose, ...)
  }
}

safe_xgb_train <- function(data, verbose = 0, ...) {
  if (packageVersion("xgboost") >= "2.0") {
    # For xgb.train, verbosity should be in params
    xgb.train(data = data, params = list(verbosity = verbose), ...)
  } else {
    xgb.train(data = data, verbose = verbose, ...)
  }
}

# Set seed for reproducibility
set.seed(123)

# ==============================================================================
# 1. Purely Numeric Data
# ==============================================================================
message("\n==============================================================================")
message("1. Testing with Purely Numeric Data")
message("==============================================================================")

# Generate synthetic numeric data
n <- 1000
p <- 5
X_num <- as.data.table(matrix(rnorm(n * p), ncol = p))
colnames(X_num) <- paste0("x", 1:p)
y_num <- X_num$x1 + X_num$x2^2 + rnorm(n)

# Split into train and explain
train_idx <- 1:800
X_train_num <- X_num[train_idx, ]
y_train_num <- y_num[train_idx]
X_explain_num <- X_num[-train_idx, ][1:5, ] # Explain 5 observations

# ------------------------------------------------------------------------------
# A. xgboost::xgboost with data.table
# ------------------------------------------------------------------------------
message("\n--- A. xgboost::xgboost with data.table ---")
tryCatch({
  set.seed(123)
  model_num_dt <- safe_xgboost(
    data = X_train_num,
    label = y_train_num,
    nrounds = 10,
    verbose = 0,
    seed = 123
  )
  message("Prediction on head(X_train_num):")
  print(head(predict(model_num_dt, X_train_num)))

  set.seed(123)
  expl_num_dt <- explain(
    model = model_num_dt,
    x_train = X_train_num,
    x_explain = X_explain_num,
    x_train_explain = X_train_num,
    approach = "ctree",
    phi0  = mean(y_train_num)
  )
  print(expl_num_dt)
}, error = function(e) {
  message("Skipping A (xgboost with data.table) due to error: ", e$message)
})

# ------------------------------------------------------------------------------
# B. xgboost::xgboost with matrix
# ------------------------------------------------------------------------------
message("\n--- B. xgboost::xgboost with matrix ---")
tryCatch({
  set.seed(123)
  model_num_mat <- safe_xgboost(
    data = as.matrix(X_train_num),
    label = y_train_num,
    nrounds = 10,
    verbose = 0
  )
  message("Prediction on head(as.matrix(X_train_num)):")
  print(head(predict(model_num_mat, as.matrix(X_train_num))))

  set.seed(123)
  expl_num_mat <- explain(
    model = model_num_mat,
    x_train = X_train_num,
    x_explain = X_explain_num,
    approach = "empirical",
    phi0 = mean(y_train_num)
  )
  print(expl_num_mat)
}, error = function(e) {
  message("Skipping B (xgboost with matrix) due to error: ", e$message)
})

# ------------------------------------------------------------------------------
# C. xgboost::xgb.train with xgb.DMatrix
# ------------------------------------------------------------------------------
message("\n--- C. xgboost::xgb.train with xgb.DMatrix ---")
tryCatch({
  dtrain_num <- xgb.DMatrix(data = as.matrix(X_train_num), label = y_train_num)
  set.seed(123)
  model_num_dmat <- safe_xgb_train(
    data = dtrain_num,
    nrounds = 10,
    verbose = 0
  )
  message("Prediction on head(dtrain_num):")
  print(head(predict(model_num_dmat, dtrain_num)))

  # Note: For xgb.train models (xgb.Booster), shapr might expect matrix input
  # depending on implementation. Passing matrix here to be safe/standard.
  set.seed(123)
  expl_num_dmat <- explain(
    model = model_num_dmat,
    x_train = X_train_num,
    x_explain = X_explain_num,
    approach = "empirical",
    phi0 = mean(y_train_num)
  )
  print(expl_num_dmat)
}, error = function(e) {
  message("Skipping C (xgb.train with xgb.DMatrix) due to error: ", e$message)
})# ------------------------------------------------------------------------------
# C2. xgboost::xgb.train with xgb.DMatrix (from data.table)
# ------------------------------------------------------------------------------
message("\n--- C2. xgboost::xgb.train with xgb.DMatrix (from data.table) ---")
tryCatch({
    dtrain_num_dt <- xgb.DMatrix(data = X_train_num, label = y_train_num)
    model_num_dmat_dt <- safe_xgb_train(
        data = dtrain_num_dt,
        nrounds = 10,
        verbose = 0
    )
    message("Prediction on head(dtrain_num_dt):")
    print(head(predict(model_num_dmat_dt, dtrain_num_dt)))

    expl_num_dmat_dt <- explain(
        model = model_num_dmat_dt,
        x_train = X_train_num,
        x_explain = X_explain_num,
        approach = "empirical",
        phi0 = mean(y_train_num)
    )
    print(expl_num_dmat_dt)
}, error = function(e) {
    message("Skipping C2 (xgb.train with xgb.DMatrix from data.table) due to error: ", e$message)
})

# ==============================================================================
# 2. Categorical / Mixed Data
# ==============================================================================
message("\n==============================================================================")
message("2. Testing with Categorical/Mixed Data")
message("==============================================================================")

# Generate synthetic mixed data
X_cat <- copy(X_num)
X_cat[, cat1 := as.factor(sample(c("A", "B", "C"), n, replace = TRUE))]
X_cat[, cat2 := as.factor(sample(c("X", "Y"), n, replace = TRUE))]
y_cat <- y_num + ifelse(X_cat$cat1 == "A", 1, 0) + ifelse(X_cat$cat2 == "X", -1, 0)

X_train_cat <- X_cat[train_idx, ]
y_train_cat <- y_cat[train_idx]
X_explain_cat <- X_cat[-train_idx, ][1:5, ]

# ------------------------------------------------------------------------------
# D. xgboost::xgboost with data.table (Native Categorical Support)
# ------------------------------------------------------------------------------
message("\n--- D. xgboost::xgboost with data.table (Factors) ---")
# Note: This requires xgboost version that supports categorical data directly (e.g. via one-hot or internal)
# We assume the environment supports it or we pass params if needed.
# For newer xgboost, we might need `params = list(enable_categorical = TRUE)`?
# Or it might handle it automatically if passed as DMatrix.
# xgboost() function usually converts to DMatrix.
tryCatch({
  set.seed(123)
  model_cat_dt <- safe_xgboost(
    data = X_train_cat,
    label = y_train_cat,
    nrounds = 10,
    verbose = 3
  )
  message("Prediction on head(X_train_cat):")
  print(head(predict(model_cat_dt, X_train_cat)))

  set.seed(123)
  expl_cat_dt <- explain(
    model = model_cat_dt,
    x_train = X_train_cat,
    x_explain = X_explain_cat,
    approach = "ctree",
    phi0 = mean(y_train_cat)
  )
  print(expl_cat_dt)
}, error = function(e) {
  message("Skipping D (xgboost with data.table factors) due to error: ", e$message)
})

# ------------------------------------------------------------------------------
# E. xgboost::xgboost with matrix (Converted to Numeric)
# ------------------------------------------------------------------------------
message("\n--- E. xgboost::xgboost with matrix (Converted to Numeric) ---")
tryCatch({
  # Matrix input must be numeric. We convert factors to integer codes.
  X_train_cat_num <- copy(X_train_cat)
  X_train_cat_num[, cat1 := as.numeric(cat1)]
  X_train_cat_num[, cat2 := as.numeric(cat2)]
  X_explain_cat_num <- copy(X_explain_cat)
  X_explain_cat_num[, cat1 := as.numeric(cat1)]
  X_explain_cat_num[, cat2 := as.numeric(cat2)]

  set.seed(123)
  model_cat_mat <- safe_xgboost(
    data = as.matrix(X_train_cat_num),
    label = y_train_cat,
    nrounds = 10,
    verbose = 0
  )
  message("Prediction on head(as.matrix(X_train_cat_num)):")
  print(head(predict(model_cat_mat, as.matrix(X_train_cat_num))))

  set.seed(123)
  expl_cat_mat <- explain(
    model = model_cat_mat,
    x_train = as.matrix(X_train_cat_num),
    x_explain = as.matrix(X_explain_cat_num),
    x_train_explain = as.matrix(X_train_cat_num),
    approach = "empirical",
    phi0 = mean(y_train_cat)
  )
  print(expl_cat_mat)
}, error = function(e) {
  message("Skipping E (xgboost with matrix converted to numeric) due to error: ", e$message)
})

# ------------------------------------------------------------------------------
# F. xgboost::xgb.train with xgb.DMatrix (Mixed)
# ------------------------------------------------------------------------------
message("\n--- F. xgboost::xgb.train with xgb.DMatrix (Mixed) ---")
# Trying to create DMatrix from data.table with factors (New Feature Support)
tryCatch({
  # This might require enable_categorical=TRUE in xgb.DMatrix or params
  dtrain_cat <- xgb.DMatrix(data = X_train_cat, label = y_train_cat, enable_categorical = TRUE)

  set.seed(123)
  model_cat_dmat <- xgboost::xgb.train(
    data = dtrain_cat,
    nrounds = 10,
    params = list(verbosity= 3)
  )
  message("Prediction on head(dtrain_cat):")
  print(head(predict(model_cat_dmat, dtrain_cat)))

  # For explain(), we pass the original data.table with factors.
  # shapr needs to handle this correctly with the model.
  set.seed(123)
  expl_cat_dmat <- explain(
    model = model_cat_dmat,
    x_train = X_train_cat,
    x_explain = X_explain_cat,
    approach = "ctree",
    phi0 = mean(y_train_cat)
  )
  print(expl_cat_dmat)
}, error = function(e) {
  message("Skipping F (xgb.train with DMatrix factors) due to error: ", e$message)
})

message("\nDone.")




  set.seed(123)
  model_cat_dt <- safe_xgboost(
    data = X_train_cat,
    label = y_train_cat,
    nrounds = 10,
    verbose = 0,
    params = list(enable_categorical = TRUE) # Uncomment if needed for specific version
  )


### v1

  dtrain_cat <- xgb.DMatrix(data = X_train_cat, label = y_train_cat)
  model_cat_dmat <- xgboost::xgb.train(
    data = dtrain_cat,
    nrounds = 10,
    params = list(verbosity= 3)
  )
    print(head(predict(model_cat_dmat, dtrain_cat)))

### v2

  model_cat_dt <- xgboost::xgboost(
    x = X_train_cat,
    y = y_train_cat,
    nrounds = 10,
    verbosity = 3
  )

    print(head(predict(model_cat_dt, X_train_cat)))


