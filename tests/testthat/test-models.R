library(shapr)
library(testthat)

context("test-models.R")

test_that("Test predict_model (regression)", {

  # Data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"
  x_train <- tail(Boston[, x_var], -6)
  y_train <- tail(Boston[, y_var], -6)
  x_test <- head(Boston[, x_var], 6)
  str_formula <- "y_train ~ lstat + rm + dis + indus"
  train_df <- cbind(y_train, x_train)

  # List of models
  l <- list(
    stats::lm(str_formula, data = train_df),
    stats::glm(str_formula, data = train_df),
    ranger::ranger(str_formula, data = train_df),
    xgboost::xgboost(data = as.matrix(x_train), label = y_train, nrounds = 3, verbose = FALSE),
    mgcv::gam(as.formula(str_formula), data = train_df)
  )

  # Tests
  for (i in seq_along(l)) {

    # Input equals data.frame
    expect_true(
      is.vector(predict_model(l[[i]], x_test))
    )
    expect_true(
      is.atomic(predict_model(l[[i]], x_test))
    )
    expect_true(
      is.double(predict_model(l[[i]], x_test))
    )
    expect_true(
      length(predict_model(l[[i]], x_test)) == nrow(x_test)
    )

    # Input equals matrix
    expect_true(
      is.double(predict_model(l[[i]], as.matrix(x_test)))
    )
    expect_true(
      is.atomic(predict_model(l[[i]], as.matrix(x_test)))
    )
    expect_true(
      is.vector(predict_model(l[[i]], as.matrix(x_test)))
    )
    expect_true(
      length(predict_model(l[[i]], as.matrix(x_test))) == nrow(x_test)
    )

    # Check that output is equal
    expect_equal(
      predict_model(l[[i]], x_test), predict_model(l[[i]], as.matrix(x_test))
    )

    # Check model type
    expect_equal(
      model_type(l[[i]]), "regression"
    )
  }
})

test_that("Test predict_model (binary classification)", {

  # Data -----------
  data("iris", package = "datasets")
  x_var <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y_var <- "Species"
  iris$Species <- as.character(iris$Species)
  iris <- iris[which(iris$Species != "virginica"), ]
  iris$Species <- as.factor(iris$Species)
  x_train <- tail(iris[, x_var], -6)
  y_train <- tail(iris[, y_var], -6)
  x_test <- head(iris[, x_var], 6)
  str_formula <- "y_train ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
  train_df <- cbind(y_train, x_train)

  # List of models
  l <- list(
    suppressWarnings(stats::glm(str_formula, data = train_df, family = "binomial")),
    suppressWarnings(mgcv::gam(as.formula(str_formula), data = train_df, family = "binomial")),
    ranger::ranger(str_formula, data = train_df, probability = TRUE),
    xgboost::xgboost(
      data = as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "binary:logistic",
      eval_metric = "error"
    )
  )

  # Tests
  for (i in seq_along(l)) {

    # Input equals data.frame
    expect_true(
      is.vector(predict_model(l[[i]], x_test))
    )
    expect_true(
      is.atomic(predict_model(l[[i]], x_test))
    )
    expect_true(
      is.double(predict_model(l[[i]], x_test))
    )
    expect_true(
      length(predict_model(l[[i]], x_test)) == nrow(x_test)
    )
    expect_true(
      all(data.table::between(predict_model(l[[i]], x_test), 0, 1))
    )

    # Input equals matrix
    expect_true(
      is.double(predict_model(l[[i]], as.matrix(x_test)))
    )
    expect_true(
      is.atomic(predict_model(l[[i]], as.matrix(x_test)))
    )
    expect_true(
      is.vector(predict_model(l[[i]], as.matrix(x_test)))
    )
    expect_true(
      length(predict_model(l[[i]], as.matrix(x_test))) == nrow(x_test)
    )
    expect_true(
      all(data.table::between(predict_model(l[[i]], as.matrix(x_test)), 0, 1))
    )

    # Check that output is equal
    expect_equal(
      predict_model(l[[i]], x_test), predict_model(l[[i]], as.matrix(x_test))
    )

    # Check model type
    expect_equal(
      model_type(l[[i]]), "classification"
    )
  }

  # Errors
  l <- list(
    ranger::ranger(
      str_formula,
      data = train_df
    ),
    xgboost::xgboost(
      data = as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "reg:logistic"
    )
  )

  # Tests
  for (i in seq_along(l)) {

    # Input equals data.frame
    expect_error(
      predict_model(l[[i]], x_test)
    )

    # Input equals matrix
    expect_error(
      predict_model(l[[i]], as.matrix(x_test))
    )

    # Check model type
    expect_error(
      model_type(l[[i]])
    )
  }
})

test_that("Test predict_model (multi-classification)", {

  # Data -----------
  data("iris", package = "datasets")
  x_var <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y_var <- "Species"
  x_train <- tail(iris[, x_var], -6)
  y_train <- tail(iris[, y_var], -6)
  x_test <- head(iris[, x_var], 6)
  str_formula <- "y_train ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
  train_df <- cbind(y_train, x_train)

  # List of models
  l <- list(
    ranger::ranger(
      str_formula,
      data = train_df
    ),
    ranger::ranger(
      str_formula,
      data = train_df,
      probability = TRUE
    ),
    xgboost::xgboost(
      as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "multi:softprob",
      eval_metric = "merror",
      num_class = 3
    ),
    xgboost::xgboost(
      as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "multi:softmax",
      eval_metric = "merror",
      num_class = 3
    )
  )

  # Tests
  for (i in seq_along(l)) {

    # Input equals data.frame
    expect_error(
      predict_model(l[[i]], x_test)
    )

    # Input equals matrix
    expect_error(
      predict_model(l[[i]], as.matrix(x_test))
    )

    # Check model type
    expect_error(
      model_type(l[[i]])
    )
  }
})

test_that("Test features (regression)", {

  # Data -----------
  data("Boston", package = "MASS")
  y_var <- "medv"
  x_train <- tail(Boston, -6)
  y_train <- tail(Boston[, y_var], -6)

  # convert to factors for testing purposes
  x_train$rad <- factor(round(x_train$rad))
  x_train$chas <- factor(round(x_train$chas))

  train_df <- cbind(x_train, y_train)

  x_var_numeric <- c("lstat", "rm", "dis", "indus")
  x_var_factor <- c("lstat", "rm", "dis", "indus", "rad", "chas")


  formula_numeric <- as.formula(paste0("y_train ~ ",paste0(x_var_numeric,collapse="+")))
  formula_factor <- as.formula(paste0("y_train ~ ",paste0(x_var_factor,collapse="+")))


  # List of models to run silently
  l_silent <- list(
    stats::lm(formula_numeric, data = train_df),
    stats::glm(formula_numeric, data = train_df),
    mgcv::gam(formula_numeric, data = train_df),
    stats::lm(formula_factor, data = train_df),
    stats::glm(formula_factor, data = train_df),
    mgcv::gam(formula_factor, data = train_df),
    xgboost::xgboost(data = dummylist$train_dummies, label = y_train, nrounds = 3, verbose = FALSE)

  )
  dummylist <- make_dummies(traindata = x_train[, x_var_factor], testdata = x_train[, x_var_factor])

  l_silent[[7]]$dummylist <- dummylist



  l_message <- list(
    ranger::ranger(formula_numeric, data = train_df),
    xgboost::xgboost(data = as.matrix(x_train[, x_var_numeric]), label = y_train, nrounds = 3, verbose = FALSE),

    ranger::ranger(formula_factor, data = train_df)
    )


  data_features <- get_data_features(train_df)
  for (i in seq_along(l_silent)) {
    model_features <- get_model_features(l_silent[[i]])
    expect_silent(check_features(model_features,data_features))
  }

  for (i in seq_along(l_message)) {
    model_features <- get_model_features(l_message[[i]])
    expect_message(check_features(model_features,data_features))
  }


  # Checking all stops in check_features
  data_features_ok <- get_data_features(train_df)

  # Non-matching labels
  data_features_error <- get_data_features(train_df)
  data_features_error$labels <- NULL
  expect_error(check_features(data_features_ok,data_features_error))
  expect_error(check_features(data_features_error,data_features_ok))

  # Missing features
  data_features_error <- get_data_features(train_df[,-3])
  expect_error(check_features(data_features_ok,data_features_error))
  expect_error(check_features(data_features_error,data_features_ok,use_1_as_truth = F))

  # Duplicated column names
  data_features_error <- get_data_features(cbind(crim=train_df[,1],train_df))
  expect_error(check_features(data_features_error,data_features_error))

  # Empty column names
  train_df_0 <- train_df
  names(train_df_0)[1] = ""
  data_features_error <- get_data_features(train_df_0)
  expect_error(check_features(data_features_error,data_features_error))

  # feature class is NA
  data_features_error <- data_features_ok
  data_features_error$classes <- rep(NA,length(data_features_error$classes))
  expect_message(check_features(data_features_error,data_features_error))

  # feature classes are different
  data_features_error <- data_features_ok
  data_features_error$classes <- rev(data_features_error$classes)
  names(data_features_error$classes) <- names(model_features_ok$classes)
  expect_error(check_features(model_features_ok,data_features_error))

  # invalid feature class
  data_features_error <- data_features_ok
  data_features_error$classes[1] <- "logical"
  expect_error(check_features(data_features_error,data_features_error))

  # non-matching factor levels
  data_features_error <- data_features_ok
  data_features_error$factor_levels$chas <- c(data_features_error$factor_levels$chas,"2")
  expect_error(check_features(data_features_ok,data_features_error))


})

test_that("Test features (binary classification)", {

  # Data -----------
  data("iris", package = "datasets")
  x_var <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y_var <- "Species"
  iris$Species <- as.character(iris$Species)
  iris <- iris[which(iris$Species != "virginica"), ]
  iris$Species <- as.factor(iris$Species)
  x_train <- tail(iris, -6)
  y_train <- tail(iris[, y_var], -6)
  str_formula <- "y_train ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
  train_df <- cbind(y_train, x_train)

  # List of models
  l <- list(
    suppressWarnings(stats::glm(str_formula, data = train_df, family = "binomial")),
    suppressWarnings(mgcv::gam(as.formula(str_formula), data = train_df, family = "binomial")),
    ranger::ranger(str_formula, data = train_df, probability = TRUE),
    xgboost::xgboost(
      data = as.matrix(x_train[, x_var]),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "binary:logistic",
      eval_metric = "error",
    )
  )

  for (i in seq_along(l)) {
    expect_equal(features(l[[i]], cnms = colnames(train_df)), x_var)
  }


})

test_that("Test missing colnames", {

  # Data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"
  x_train <- as.matrix(tail(Boston[, x_var], -6))
  y_train <- tail(Boston[, y_var], -6)
  x_test <- as.matrix(head(Boston[, x_var]))

  x_train_nonames <- x_train
  colnames(x_train_nonames) <- NULL
  x_test_nonames <- x_test
  colnames(x_test_nonames) <- NULL

  model <- xgboost::xgboost(
    data = x_train, label = y_train, nrounds = 3, verbose = FALSE
    )
  model_nonames <- xgboost::xgboost(
    data = x_train_nonames, label = y_train, nrounds = 3, verbose = FALSE
  )

  # missing colnames in model
  expect_error(shapr(model_nonames, x_train))

  # missing colnames in training data
  expect_error(shapr(model, x_train_nonames))

  # missing colnames in both model and training data
  expect_error(shapr(model_nonames, x_train_nonames))

  # missing colnames in test data
  explain <- shapr(x_train, model)
  p <- mean(y_train)
  expect_error(
    explain(
      x_test_nonames,
      approach = "empirical",
      explainer = explainer,
      prediction_zero = p
    )
  )

})
