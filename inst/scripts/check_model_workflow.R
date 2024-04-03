# Compare xgboost with parsnip version ----------------------------------------------------------------------------
# Either use library(tidymodels) or separately specify the libraries
library(parsnip)
library(ggplot2)
library(recipes)
library(workflows)
library(dials)
library(hardhat)
library(workflows)
library(yardstick)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"
all_var <- c(y_var, x_var)

ind_x_explain <- 1:20
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]
train <- data[-ind_x_explain, ..all_var]
test <- data[ind_x_explain, ..all_var]

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Fitting a basic xgboost model to the training data using tidymodels
set.seed(1)
model_xgboost <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 10,
  verbose = FALSE
)

set.seed(1)
model_workflow <- workflows::workflow() %>%
  workflows::add_model(parsnip::boost_tree(trees = 10, engine = "xgboost", mode = "regression")) %>%
  workflows::add_recipe(recipes::recipe(Ozone ~ ., data = train)) %>%
  parsnip::fit(data = test)

# See that the predictions are identical
all.equal(predict(model_workflow, x_train)$.pred, predict(model_xgboost, as.matrix(x_train)))

explain_workflow = explain(
  model = model_workflow,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = p0,
  n_batches = 4
)

explain_xgboost = explain(
  model = model_xgboost,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = p0,
  n_batches = 4
)

# See that the shapley values are identical
all.equal(explain_workflow$shapley_values, explain_xgboost$shapley_values)

# Other models in workflow ---------------------------------------------------------------------------------------------
set.seed(1)
data <- data.table::as.data.table(airquality)
data[, Month_factor := as.factor(Month)]
data[, Ozone_sub30 := (Ozone < 30) * 1]
data[, Ozone_sub30_factor := as.factor(Ozone_sub30)]
data[, Solar.R_factor := as.factor(cut(Solar.R, 10))]
data[, Wind_factor := as.factor(round(Wind))]

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(seq_len(.N))] # Sh

x_var_mixed <- c("Solar.R", "Wind", "Temp", "Day", "Month_factor")
var_mixed <- c("Ozone", x_var_mixed)

data_train <- head(data_complete, -3)
data_explain <- tail(data_complete, 3)

x_train_mixed <- data_train[, ..x_var_mixed]
x_explain_mixed <- data_explain[, ..x_var_mixed]
train_mixed <- data_train[, ..var_mixed]

model_decision_tree <- workflows::workflow() %>%
  workflows::add_model(parsnip::decision_tree(engine = "rpart", mode = "regression")) %>%
  workflows::add_recipe(recipes::recipe(Ozone ~ ., data = train_mixed) %>%
                          recipes::step_dummy(all_factor_predictors())) %>%
  parsnip::fit(data = train_mixed)

y_var_numeric <- "Ozone"
lm_formula_mixed <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_mixed, collapse = " + ")))
model_lm_mixed <- lm(lm_formula_mixed, data = data_complete)

explain_decision_tree_ctree = explain(
  model = model_decision_tree,
  x_explain = x_explain_mixed,
  x_train = x_train_mixed,
  approach = "ctree",
  prediction_zero = p0,
  n_batches = 4
)

explain_decision_tree_lm = explain(
  model = model_decision_tree, #model_lm_mixed
  x_explain = x_explain_mixed,
  x_train = x_train_mixed,
  approach = "regression_separate",
  regression.model = parsnip::linear_reg(),
  prediction_zero = p0,
  n_batches = 4
)

#

# CV -------------------------------------------------------------------------------------------------------------------
set.seed(1)
regression.workflow <- workflows::workflow() %>%
  workflows::add_model(parsnip::rand_forest(
   trees = hardhat::tune(), engine = "ranger", mode = "regression"
  )) %>%
  workflows::add_recipe(recipes::recipe(Ozone ~ ., data = train_mixed) %>%
                          recipes::step_dummy(all_factor_predictors()))

# Add the hyperparameter tuning to the workflow
regression.results <- tune::tune_grid(
  object = regression.workflow,
  resamples = rsample::vfold_cv(data = train_mixed, v = 3),
  grid = dials::grid_regular(dials::trees(c(50, 750)), levels = 3),
  metrics = yardstick::metric_set(yardstick::rmse)
)

# Update the workflow by finalizing it using the hyperparameters that attained the best rmse
regression.workflow <- tune::finalize_workflow(regression.workflow, tune::select_best(regression.results, "rmse"))

# Fit the model to the augmented training data
model_rf_cv <- parsnip::fit(regression.workflow, data = train_mixed)

# See that the model works with regression
explain_decision_model_rf_cv_rf = explain(
  model = model_rf_cv, #model_lm_mixed
  x_explain = x_explain_mixed,
  x_train = x_train_mixed,
  approach = "regression_separate",
  regression.model = parsnip::rand_forest(engine = "ranger", mode = "regression"),
  prediction_zero = p0,
  n_batches = 4
)

# See that the model works with MC method too
explain_decision_model_rf_cv_ctree = explain(
  model = model_rf_cv, #model_lm_mixed
  x_explain = x_explain_mixed,
  x_train = x_train_mixed,
  approach = "ctree",
  prediction_zero = p0,
  n_batches = 4
)

# Quite similar
plot_MSEv_eval_crit(list(ctree = explain_decision_model_rf_cv_ctree, rf = explain_decision_model_rf_cv_rf))
plot_SV_several_approaches(list(ctree = explain_decision_model_rf_cv_ctree, rf = explain_decision_model_rf_cv_rf))

