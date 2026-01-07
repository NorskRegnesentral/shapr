# More focused example showing XGBoost tuning error
# This version makes the error more explicit

.libPaths("/home/jullum/R/x86_64-pc-linux-gnu-library/4.3")
#.libPaths("/home/jullum/R/x86_64-pc-linux-gnu-library/4.3_old_xgboost")


library(parsnip)
library(tune)
library(workflows)
library(recipes)
library(rsample)
library(yardstick)
library(hardhat)

library(xgboost)
# Print XGBoost version
print(paste0("XGBoost version:", packageVersion("xgboost")))


# Create simple data
set.seed(42)
data <- data.frame(
  x1 = rnorm(50),
  x2 = rnorm(50),
  y = rnorm(50)
)

# Create recipe
rec <- recipe(y ~ ., data = data)

# The problematic model specification
xgb_spec <- boost_tree(
  trees = tune(),  # This parameter causes the issue
  engine = "xgboost",
  mode = "regression"
)

# Create workflow
wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

# Create small CV folds
cv_folds <- vfold_cv(data, v = 2)

# Simple grid that should trigger the error
grid <- data.frame(trees = c(5, 10))

cat("Attempting to tune XGBoost with trees parameter...\n")
cat("This should produce the 'Out of range for tree layers' error\n\n")

# Run tuning - this will produce the error
result <- tune_grid(
  wf,
  resamples = cv_folds,
  grid = grid,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE)
)

cat("\nTuning results:\n")
print(collect_metrics(result))