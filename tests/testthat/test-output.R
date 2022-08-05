library(data.table)

options(digits = 5) # To avoid round off errors when printing output on different systems

set.seed(1234)

data <- data.table::as.data.table(airquality)
data[, Month_factor := as.factor(Month)]
data[, Ozone_sub30 := (Ozone < 30) * 1]
data[, Ozone_sub30_factor := as.factor(Ozone_sub30)]

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(1:.N)] # Sh

y_var_numeric <- "Ozone"
y_var_binary <- "Ozone_sub30"
y_var_binaryfactor <- "Ozone_sub30_factor"


x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month", "Day")
x_var_mixed <- c("Solar.R", "Wind", "Temp", "Day", "Month_factor")

data_train <- head(data_complete, -2)
data_test <- tail(data_complete, 2)

x_train_numeric <- data_train[, ..x_var_numeric]
x_train_mixed <- data_train[, ..x_var_mixed]

x_test_numeric <- data_test[, ..x_var_numeric]
x_test_mixed <- data_test[, ..x_var_mixed]

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))
lm_formula_mixed <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_mixed, collapse = " + ")))

model_lm_numeric <- lm(lm_formula_numeric, data = data_complete)
model_lm_mixed <- lm(lm_formula_mixed, data = data_complete)

p0 <- data_train[, mean(get(y_var_numeric))]

# lm_numeric with different approaches

test_that("output_lm_numeric_independence", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "independence", prediction_zero = p0),
    "output_lm_numeric_independence"
  )
})

test_that("output_lm_numeric_empirical", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "empirical", prediction_zero = p0),
    "output_lm_numeric_empirical"
  )
})

test_that("output_lm_numeric_empirical_AICc_each", {
  set.seed(123)
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "empirical", prediction_zero = p0, n_combinations = 8, type = "AICc_each_k"),
    "output_lm_numeric_empirical_AICc_each"
  )
})

test_that("output_lm_numeric_empirical_AICc_full", {
  set.seed(123)
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "empirical", prediction_zero = p0, n_combinations = 8, type = "AICc_full"),
    "output_lm_numeric_empirical_AICc_full"
  )
})

test_that("output_lm_numeric_gaussian", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "gaussian", prediction_zero = p0),
    "output_lm_numeric_gaussian"
  )
})

test_that("output_lm_numeric_copula", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "copula", prediction_zero = p0),
    "output_lm_numeric_copula"
  )
})

test_that("output_lm_numeric_ctree", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "ctree", prediction_zero = p0),
    "output_lm_numeric_ctree"
  )
})

test_that("output_lm_numeric_comb1", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = c("gaussian", "empirical", "ctree", "independence", "empirical"), prediction_zero = p0),
    "output_lm_numeric_comb1"
  )
})

test_that("output_lm_numeric_comb2", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = c("ctree", "copula", "independence", "copula", "empirical"), prediction_zero = p0),
    "output_lm_numeric_comb2"
  )
})

test_that("output_lm_numeric_comb3", {
  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = c("independence", "empirical", "gaussian", "empirical", "gaussian"), prediction_zero = p0),
    "output_lm_numeric_comb3"
  )
})


# lm_mixed with different approahces

test_that("output_lm_mixed_independence", {
  expect_snapshot_rds(
    explain(x_train_mixed, x_test_mixed, model_lm_mixed, approach = "independence", prediction_zero = p0),
    "output_lm_mixed_independence"
  )
})

test_that("output_lm_mixed_ctree", {
  expect_snapshot_rds(
    explain(x_train_mixed, x_test_mixed, model_lm_mixed, approach = "ctree", prediction_zero = p0),
    "output_lm_mixed_ctree"
  )
})

test_that("output_lm_mixed_comb", {
  set.seed(123)
  expect_snapshot_rds(
    explain(x_train_mixed, x_test_mixed, model_lm_mixed, approach = c("ctree", "independence", "ctree", "independence", "independence"), prediction_zero = p0),
    "output_lm_mixed_comb"
  )
})



### Custom model by passing predict_model
test_that("output_custom_lm_numeric_independence_1", {
  set.seed(123)
  custom_pred_func <- function(x, newdata) {
    beta <- coef(x)
    X <- cbind(1, newdata)
    return(as.vector(beta %*% t(X)))
  }

  model_custom_lm_numeric <- model_lm_numeric

  expect_snapshot_rds(
    explain(x_train_numeric, x_test_numeric, model_custom_lm_numeric, approach = "independence", prediction_zero = p0, predict_model = custom_pred_func),
    "output_custom_lm_numeric_independence_1"
  )
})

test_that("output_custom_lm_numeric_independence_2", {
  set.seed(123)
  custom_pred_func <- function(x, newdata) {
    beta <- coef(x)
    X <- cbind(1, newdata)
    return(as.vector(beta %*% t(X)))
  }

  model_custom_lm_numeric <- model_lm_numeric
  class(model_custom_lm_numeric) <- "whatever"


  expect_snapshot_rds(
    (custom <- explain(x_train_numeric, x_test_numeric, model_custom_lm_numeric, approach = "independence", prediction_zero = p0, predict_model = custom_pred_func)),
    "output_custom_lm_numeric_independence_2"
  )

  native <- explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "independence", prediction_zero = p0)

  # Check that the printed Shapley values are identical
  expect_equal(
    custom$shapley_values,
    native$shapley_values
  )
})

test_that("output_custom_xgboost_mixed_dummy_ctree", {
  if (requireNamespace("xgboost", quietly = TRUE)) {
    x_train_mixed_dummy <- model.matrix(~.+0,x_train_mixed)
    x_test_mixed_dummy <- model.matrix(~.+0,x_test_mixed)

    y_train <- data_train[, get(y_var_numeric)]

    # Fitting a basic xgboost model to the training data
    model_xgboost_mixed_dummy <- xgboost::xgboost(
      data = x_train_mixed_dummy,
      label = y_train,
      nround = 20,
      verbose = FALSE
    )

    predict_model.xgboost_dummy <- function(x,newdata){
      newdata_dummy <- model.matrix(~.+0,newdata)

      predict(x, newdata_dummy)
    }

    # Check that created predict_model works as intended
    expect_equal(
      predict_model.xgboost_dummy(model_xgboost_mixed_dummy,x_test_mixed),
      predict(model_xgboost_mixed_dummy,x_test_mixed_dummy)
    )

    # Specifying the phi_0, i.e. the expected prediction without any features
    p0 <- data_train[, mean(get(y_var_numeric))]


    expect_snapshot_rds(
      explain(x_train_mixed, x_test_mixed, model_xgboost_mixed_dummy, approach = "ctree", prediction_zero = p0, predict_model = predict_model.xgboost_dummy, get_model_specs = NA),
      "output_custom_xgboost_mixed_dummy_ctree"
    )

  }
})


