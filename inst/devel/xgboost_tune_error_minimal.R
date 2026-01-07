# Minimal example to reproduce XGBoost tuning error with parsnip
# This script demonstrates the issue without any shapr involvement

library(parsnip)
library(tune)
library(workflows)
library(recipes)
library(rsample)
library(yardstick)
library(dials)

# Create simple synthetic data
set.seed(123)
n <- 100
data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  y = rnorm(n)
)

# Split into train/test
train_data <- data[1:80, ]
test_data <- data[81:100, ]

# Create recipe
rec <- recipe(y ~ ., data = train_data)

# Create model specification with tuning
xgb_spec <- boost_tree(
  trees = tune(),
  engine = "xgboost",
  mode = "regression"
)

# Create workflow
xgb_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

# Create cross-validation folds
cv_folds <- vfold_cv(train_data, v = 3)

# Create tuning grid
tune_grid <- expand.grid(trees = c(10, 20))

# This should produce the XGBoost error
cat("Starting hyperparameter tuning...\n")
tryCatch({
  tune_results <- tune_grid(
    xgb_wf,
    resamples = cv_folds,
    grid = tune_grid,
    metrics = metric_set(rmse)
  )

  # If we get here, try to fit the best model
  best_params <- select_best(tune_results, metric = "rmse")
  final_wf <- finalize_workflow(xgb_wf, best_params)
  final_fit <- fit(final_wf, train_data)

  # Try prediction which might also trigger the error
  preds <- predict(final_fit, test_data)
  cat("Success! No error occurred.\n")

}, error = function(e) {
  cat("Error occurred:\n")
  cat(conditionMessage(e), "\n")
})