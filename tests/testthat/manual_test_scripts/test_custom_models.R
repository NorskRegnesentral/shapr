# Test custom models

# Doing all testing from shapr
# Because new functions have to be created (to use gbm with shapr), we cannot use a classic testthat set up because
# shapr will not see the functions created inside of the test environment. Therefore we have to test these functions
# a bit differently (and more manual) than other tests.

library(testthat)
library(shapr)
library(gbm)
library(MASS)

# Data -----------
data("Boston", package = "MASS")
y_var <- "medv"
x_train <- tail(Boston, -6)
y_train <- tail(Boston[, y_var], -6)
y_train_binary <- as.factor(tail((Boston[, y_var] > 20) * 1, -6))

# convert to factors for testing purposes
x_train$rad <- factor(round(x_train$rad))
x_train$chas <- factor(round(x_train$chas))

train_df <- cbind(x_train, y_train, y_train_binary)


x_var_numeric <- c("lstat", "rm", "dis", "indus")
x_var_factor <- c("lstat", "rm", "dis", "indus", "rad", "chas")

train_df_used_numeric <- x_train[, x_var_numeric]
train_df_used_factor <- x_train[, x_var_factor]

formula_numeric <- as.formula(paste0("y_train ~ ", paste0(x_var_numeric, collapse = "+")))
formula_factor <- as.formula(paste0("y_train ~ ", paste0(x_var_factor, collapse = "+")))

# Custom model with only numeric features
model_custom <- gbm::gbm(formula_numeric, data = train_df, distribution = "gaussian")
expect_error(shapr(train_df_used_numeric, model_custom)) # Required model objects defined
get_model_specs.gbm <- function(x) {
  feature_list <- list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)
  feature_list$classes <- attr(x$Terms, "dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes == "factor"] <- NA # the model object don't contain factor levels info
  return(feature_list)
}
expect_error(shapr(train_df_used_numeric, model_custom)) # predict_model objects not defined

predict_model.gbm <- function(x, newdata) {
  if (!requireNamespace("gbm", quietly = TRUE)) {
    stop("The gbm package is required for predicting train models")
  }
  model_type <- ifelse(
    x$distribution$name %in% c("bernoulli", "adaboost"),
    "classification",
    "regression"
  )
  if (model_type == "classification") {
    predict(x, as.data.frame(newdata), type = "response", n.trees = x$n.trees)
  } else {
    predict(x, as.data.frame(newdata), n.trees = x$n.trees)
  }
}

expect_silent(shapr(train_df_used_numeric, model_custom)) # Both defined, so pass silently

rm(get_model_specs.gbm)

expect_message(shapr(train_df_used_numeric, model_custom)) # Only predict_model defined, so warning
rm(predict_model.gbm)


# Custom model with factors
model_custom <- gbm::gbm(formula_factor, data = train_df, distribution = "gaussian")
expect_error(shapr(train_df_used_factor, model_custom)) # Required model objects defined
get_model_specs.gbm <- function(x) {
  feature_list <- list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)
  feature_list$classes <- attr(x$Terms, "dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes == "factor"] <- NA # model object doesn't contain factor level info
  return(feature_list)
}
expect_error(shapr(train_df_used_factor, model_custom)) # predict_model objects not defined

predict_model.gbm <- function(x, newdata) {
  if (!requireNamespace("gbm", quietly = TRUE)) {
    stop("The gbm package is required for predicting train models")
  }
  model_type <- ifelse(
    x$distribution$name %in% c("bernoulli", "adaboost"),
    "classification",
    "regression"
  )
  if (model_type == "classification") {
    predict(x, as.data.frame(newdata), type = "response", n.trees = x$n.trees)
  } else {
    predict(x, as.data.frame(newdata), n.trees = x$n.trees)
  }
}
expect_message(shapr(train_df_used_factor, model_custom)) # Both defined, so pass with message as factor_level is NA

rm(get_model_specs.gbm)

expect_message(shapr(train_df_used_factor, model_custom)) # Only predict_model defined, so warning message returned

rm(predict_model.gbm)

predict_model.gbm <- function(x, newdata) NULL

# Erroneous predict_model defined, so throw error + messages
expect_message(expect_error(shapr(train_df_used_factor, model_custom)))


rm(predict_model.gbm)
