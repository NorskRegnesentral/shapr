library(testthat)
library(shapr)

context("test-shapley.R")

RNGversion(vstr = "3.5.0")

test_that("Basic test functions in shapley.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  x_train <- tail(Boston[, x_var], 50)

  # Load premade lm model. Path needs to be relative to testthat directory in the package
  model <- readRDS("model_objects/lm_model_object.rds")

  # Prepare the data for explanation
  explainer <- shapr(x_train, model)

  expect_known_value(explainer, file = "test_objects/shapley_explainer_obj.rds",
                     update = F)
})


test_that("Testing data input to shapr in shapley.R", {

  # Data -----------
  data("Boston", package = "MASS")
  y_var <- "medv"
  x_train <- tail(Boston, -6)
  y_train <- tail(Boston[, y_var], -6)
  y_train_binary <- as.factor(tail((Boston[, y_var]>20)*1, -6))

  # convert to factors for testing purposes
  x_train$rad <- factor(round(x_train$rad))
  x_train$chas <- factor(round(x_train$chas))

  train_df <- cbind(x_train, y_train,y_train_binary)


  x_var_numeric <- c("lstat", "rm", "dis", "indus")
  x_var_factor <- c("lstat", "rm", "dis", "indus", "rad", "chas")

  train_df_used_numeric <- x_train[,x_var_numeric]
  train_df_used_factor <- x_train[,x_var_factor]

  formula_numeric <- as.formula(paste0("y_train ~ ",paste0(x_var_numeric,collapse="+")))
  formula_factor <- as.formula(paste0("y_train ~ ",paste0(x_var_factor,collapse="+")))

  formula_binary_numeric <- as.formula(paste0("y_train_binary ~ ",paste0(x_var_numeric,collapse="+")))
  formula_binary_factor <- as.formula(paste0("y_train_binary ~ ",paste0(x_var_factor,collapse="+")))

  dummylist <- make_dummies(traindata = x_train[, x_var_factor], testdata = x_train[, x_var_factor])

  # List of models to run silently
  l_numeric <- list(
    stats::lm(formula_numeric, data = train_df),
    stats::glm(formula_numeric, data = train_df),
    mgcv::gam(formula_numeric, data = train_df))

  l_factor <- list(
    stats::lm(formula_factor, data = train_df),
    stats::glm(formula_factor, data = train_df),
    mgcv::gam(formula_factor, data = train_df)
  )

  l_message <- list(
    xgboost::xgboost(data = dummylist$train_dummies, label = y_train,
                     nrounds = 3, verbose = FALSE)
  )

  l_message[[1]]$dummylist <- dummylist$obj




  for (i in seq_along(l_numeric)) {
    expect_silent(shapr(train_df_used_numeric,l_numeric[[i]])) # No modification
    expect_message(shapr(train_df,l_numeric[[i]])) # Features dropped
  }

  for (i in seq_along(l_factor)) {
    expect_silent(shapr(train_df_used_factor,l_factor[[i]])) # No modification
    expect_message(shapr(train_df,l_factor[[i]])) # Features dropped
  }

  for (i in seq_along(l_message)) {
    expect_message(shapr(train_df_used_factor,l_message[[i]])) # Factor reordering
    expect_message(shapr(train_df,l_message[[i]])) # Factor reordering + features dropped
  }

  # Custom model
  model_custom <- gbm::gbm(formula_factor,data = train_df,distribution = "gaussian")
  expect_error(shapr(train_df_used_factor ,model_custom)) # Both required model objects not defined

  # Create custom function of model_type for gbm
  model_type.gbm <- function(x) "regression"

  expect_error(shapr(train_df_used_factor ,model_custom)) # predict_model objects not defined

  # Create custom function of predict_model for gbm
  predict_model.gbm <- function(x, newdata) {
    predict(x, as.data.frame(newdata), n.trees = x$n.trees)
  }

  expect_error(shapr(train_df_used_factor ,model_custom)) # Missing feature_labels & get_model_specs
  expect_message(shapr(train_df_used_factor ,model_custom,feature_labels = labels(model_custom$Terms))) # feature_labels, but no get_model_specs

  expect_message(expect_error(shapr(train_df_used_factor,model_custom,
                                    feature_labels = labels(model_custom$Terms)[-1]))) # Passing invalid feature_labels

  get_model_specs.gbm <- function(x, feature_labels = NULL) {

    feature_list = list()
    feature_list$labels <- labels(x$Terms)
    m <- length(feature_list$labels)

    feature_list$classes <- attr(x$Terms,"dataClasses")[-1]
    feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
    feature_list$factor_levels[feature_list$classes=="factor"] <- x$var.levels[feature_list$classes=="factor"]

    return(feature_list)
  }
  expect_message(shapr(train_df_used_factor ,model_custom,feature_labels = labels(model_custom$Terms))) # Both feature_labels & get_model_specs
  expect_silent(shapr(train_df_used_factor,model_custom)) # Missing feature_labels, but get_model_specs is added

  expect_message(shapr(train_df,model_custom)) # Features dropped

  # Testing errors on incompatible model and data
  # Missing features
  model <- stats::lm(formula_factor, data = train_df)
  data_error <- train_df[,-3]
  expect_error(shapr(data_error,model))

  # Duplicated column names
  data_error <- train_df_used_factor
  data_error <- cbind(data_error,lstat = 1)
  expect_error(shapr(data_error,model))

  # Empty column names in data
  tmp <- dummylist$train_dummies
  colnames(tmp) <- NULL
  model_xgb <- xgboost::xgboost(data = tmp, label = y_train,
                            nrounds = 3, verbose = FALSE)
  data_error <- train_df
  expect_error(shapr(data_error,model_xgb))

  # Data feature with incorrect class
  data_error <- train_df_used_factor
  data_error$lstat <- as.integer(data_error$lstat>15)
  expect_error(shapr(data_error,model))

  # non-matching factor levels
  data_error <- head(train_df_used_factor)
  data_error$rad <- droplevels(data_error$rad)
  expect_error(shapr(data_error,model))

})
