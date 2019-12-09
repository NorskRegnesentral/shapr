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
      objective = "binary:logistic"
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
      num_class = 3
    ),
    xgboost::xgboost(
      as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "multi:softmax",
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
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"
  x_train <- tail(Boston, -6)
  y_train <- tail(Boston[, y_var], -6)
  str_formula <- "y_train ~ lstat + rm + dis + indus"
  train_df <- cbind(y_train, x_train)

  # List of models
  l <- list(
    stats::lm(str_formula, data = train_df),
    stats::glm(str_formula, data = train_df),
    ranger::ranger(str_formula, data = train_df),
    xgboost::xgboost(data = as.matrix(x_train[, x_var]), label = y_train, nrounds = 3, verbose = FALSE),
    mgcv::gam(as.formula(str_formula), data = train_df)
  )

  for (i in seq_along(l)) {
    expect_equal(features(l[[i]], cnms = colnames(train_df)), x_var)
  }

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
      objective = "binary:logistic"
    )
  )

  for (i in seq_along(l)) {
    expect_equal(features(l[[i]], cnms = colnames(train_df)), x_var)
  }

})
