context("test-models.R")

test_that("Test predict_model (regression)", {

  # Data -----------
  if (requireNamespace("MASS", quietly = TRUE)) {
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
      stats::glm(str_formula, data = train_df))

    if (requireNamespace("ranger", quietly = TRUE)) {
      l[[length(l) + 1]] <- ranger::ranger(str_formula, data = train_df)
    }
    if (requireNamespace("xgboost", quietly = TRUE)) {
      l[[length(l) + 1]] <- xgboost::xgboost(data = as.matrix(x_train), label = y_train, nrounds = 3, verbose = FALSE)
    }
    if (requireNamespace("mgcv", quietly = TRUE)) {
      l[[length(l) + 1]] <- mgcv::gam(as.formula(str_formula), data = train_df)
    }

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
    suppressWarnings(stats::glm(str_formula, data = train_df, family = "binomial")))

  if (requireNamespace("mgcv", quietly = TRUE)) {
    l[[length(l) + 1]] <- suppressWarnings(mgcv::gam(as.formula(str_formula), data = train_df, family = "binomial"))
  }
  if (requireNamespace("ranger", quietly = TRUE)) {
    l[[length(l) + 1]] <- ranger::ranger(str_formula, data = train_df, probability = TRUE)
  }
  if (requireNamespace("xgboost", quietly = TRUE)) {
    l[[length(l) + 1]] <- xgboost::xgboost(
      data = as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "binary:logistic"
    )
  }

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
  l <- list()

  if (requireNamespace("ranger", quietly = TRUE)) {
    l[[length(l) + 1]] <- ranger::ranger(str_formula, data = train_df)
  }
  if (requireNamespace("xgboost", quietly = TRUE)) {
    l[[length(l) + 1]] <- xgboost::xgboost(
      data = as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "reg:logistic")
  }

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
  l <- list()

  if (requireNamespace("ranger", quietly = TRUE)) {
    l[[length(l) + 1]] <- ranger::ranger(
      str_formula,
      data = train_df
    )
    l[[length(l) + 1]] <- ranger::ranger(
      str_formula,
      data = train_df,
      probability = TRUE
    )
  }
  if (requireNamespace("xgboost", quietly = TRUE)) {
    l[[length(l) + 1]] <- xgboost::xgboost(
      as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "multi:softprob",
      num_class = 3
    )
    l[[length(l) + 1]] <-     xgboost::xgboost(
      as.matrix(x_train),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "multi:softmax",
      num_class = 3
    )
  }


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
  if (requireNamespace("MASS", quietly = TRUE)) {
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
      stats::glm(str_formula, data = train_df)
    )

    if (requireNamespace("ranger", quietly = TRUE)) {
      l[[length(l) + 1]] <- ranger::ranger(str_formula, data = train_df)
    }
    if (requireNamespace("xgboost", quietly = TRUE)) {
      l[[length(l) + 1]] <- xgboost::xgboost(
        data = as.matrix(x_train[, x_var]),
        label = y_train,
        nrounds = 3,
        verbose = FALSE
        )
    }
    if (requireNamespace("mgcv", quietly = TRUE)) {
      l[[length(l) + 1]] <- mgcv::gam(as.formula(str_formula), data = train_df)
    }

    for (i in seq_along(l)) {
      expect_equal(features(l[[i]], cnms = colnames(train_df)), x_var)
    }

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
    suppressWarnings(stats::glm(str_formula, data = train_df, family = "binomial")))

  if (requireNamespace("mgcv", quietly = TRUE)) {
    l[[length(l) + 1]] <- suppressWarnings(mgcv::gam(as.formula(str_formula), data = train_df, family = "binomial"))
  }
  if (requireNamespace("ranger", quietly = TRUE)) {
    l[[length(l) + 1]] <- ranger::ranger(str_formula, data = train_df, probability = TRUE)
  }
  if (requireNamespace("xgboost", quietly = TRUE)) {
    l[[length(l) + 1]] <- xgboost::xgboost(
      data = as.matrix(x_train[, x_var]),
      label = as.integer(y_train) - 1,
      nrounds = 2,
      verbose = FALSE,
      objective = "binary:logistic"
    )
  }

  for (i in seq_along(l)) {
    expect_equal(features(l[[i]], cnms = colnames(train_df)), x_var)
  }


})

test_that("Test missing colnames", {

  # Data -----------
  if (requireNamespace("MASS", quietly = TRUE)) {
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

    if (requireNamespace("xgboost", quietly = TRUE)) {

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
    }
  }
})
