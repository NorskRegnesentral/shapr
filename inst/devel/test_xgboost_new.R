
#library(shapr)
devtools::load_all()
library(xgboost)
library(data.table)

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
model_num_dt <- xgboost(
  data = X_train_num,
  label = y_train_num,
  nrounds = 10,
  verbose = 0
)

expl_num_dt <- explain(
  model = model_num_dt,
  x_train = X_train_num,
  x_explain = X_explain_num,
  x_train_explain = X_train_num,
  approach = "ctree",
  phi0  = mean(y_train_num)
)
expl_num_dt

# ------------------------------------------------------------------------------
# B. xgboost::xgboost with matrix
# ------------------------------------------------------------------------------
message("\n--- B. xgboost::xgboost with matrix ---")
model_num_mat <- xgboost(
  data = as.matrix(X_train_num),
  label = y_train_num,
  nrounds = 10,
  verbose = 0
)

expl_num_mat <- explain(
  model = model_num_mat,
  x_train = X_train_num,
  x_explain = X_explain_num,
  approach = "empirical",
  phi0 = mean(y_train_num)
)
expl_num_mat


# ------------------------------------------------------------------------------
# C. xgboost::xgb.train with xgb.DMatrix
# ------------------------------------------------------------------------------
message("\n--- C. xgboost::xgb.train with xgb.DMatrix ---")
dtrain_num <- xgb.DMatrix(data = X_train_num, label = y_train_num)
model_num_dmat <- xgb.train(
  data = dtrain_num,
  nrounds = 10,
  verbose = 0
)

# Note: For xgb.train models (xgb.Booster), shapr might expect matrix input
# depending on implementation. Passing matrix here to be safe/standard.
expl_num_dmat <- explain(
  model = model_num_dmat,
  x_train = X_train_num,
  x_explain = X_explain_num,
  approach = "empirical",
  phi0 = mean(y_train_num)
)
expl_num_dmat

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
model_cat_dt <- xgboost(
data = X_train_cat,
label = y_train_cat,
    nrounds = 10,
verbose = 0,
 params = list(enable_categorical = TRUE) # Uncomment if needed for specific version
)

expl_cat_dt <- explain(
    model = model_cat_dt,
    x_train = X_train_cat,
    x_explain = X_explain_cat,
    approach = "ctree",
    phi0 = mean(y_train_cat),
)
expl_cat_dt

# ------------------------------------------------------------------------------
# F. xgboost::xgb.train with xgb.DMatrix (Mixed)
# ------------------------------------------------------------------------------
message("\n--- F. xgboost::xgb.train with xgb.DMatrix (Mixed) ---")
# Trying to create DMatrix from data.table with factors (New Feature Support)

# This might require enable_categorical=TRUE in xgb.DMatrix or params
dtrain_cat <- xgb.DMatrix(data = X_train_cat, label = y_train_cat)

model_cat_dmat <- xgb.train(
data = dtrain_cat,
nrounds = 10,
verbose = 0
)

# For explain(), we pass the original data.table with factors.
# shapr needs to handle this correctly with the model.
expl_cat_dmat <- explain(
model = model_cat_dmat,
x_train = X_train_cat,
x_explain = X_explain_cat,
approach = "ctree",
phi0 = mean(y_train_cat)
)
expl_cat_dmat

message("\nDone.")
