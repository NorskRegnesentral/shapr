context("test-shapley.R")

suppressWarnings(RNGversion(vstr = "3.5.0"))

test_that("Basic test functions in shapley.R", {

  # Load data -----------
  if (requireNamespace("MASS", quietly = TRUE)) {
    data("Boston", package = "MASS")
    x_var <- c("lstat", "rm", "dis", "indus")
    x_train <- tail(Boston[, x_var], 50)

    # Load premade lm model. Path needs to be relative to testthat directory in the package
    model <- readRDS("model_objects/lm_model_object.rds")

    # Prepare the data for explanation
    explainer <- shapr(x_train, model)

    expect_known_value(explainer,
      file = "test_objects/shapley_explainer_obj.rds",
      update = FALSE
    )
  }
})


test_that("Testing data input to shapr in shapley.R", {
  if (requireNamespace("MASS", quietly = TRUE)) {
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

    formula_binary_numeric <- as.formula(paste0("y_train_binary ~ ", paste0(x_var_numeric, collapse = "+")))
    formula_binary_factor <- as.formula(paste0("y_train_binary ~ ", paste0(x_var_factor, collapse = "+")))

    dummylist <- make_dummies(traindata = x_train[, x_var_factor], testdata = x_train[, x_var_factor])

    # List of models to run silently
    l_numeric <- list(
      stats::lm(formula_numeric, data = train_df),
      stats::glm(formula_numeric, data = train_df)
    )

    if (requireNamespace("mgcv", quietly = TRUE)) {
      l_numeric[[length(l_numeric) + 1]] <- mgcv::gam(formula_numeric, data = train_df)
    }

    l_factor <- list(
      stats::lm(formula_factor, data = train_df),
      stats::glm(formula_factor, data = train_df)
    )

    if (requireNamespace("mgcv", quietly = TRUE)) {
      l_factor[[length(l_factor) + 1]] <- mgcv::gam(formula_factor, data = train_df)
    }

    if (requireNamespace("xgboost", quietly = TRUE)) {
      l_factor[[length(l_factor) + 1]] <- xgboost::xgboost(
        data = dummylist$train_dummies,
        label = y_train,
        nrounds = 3, verbose = FALSE
      )
      l_factor[[length(l_factor)]]$feature_list <- dummylist$feature_list
    }


    for (i in seq_along(l_numeric)) {
      expect_silent(shapr(train_df_used_numeric, l_numeric[[i]])) # No modification
      expect_message(shapr(train_df, l_numeric[[i]])) # Features dropped
    }

    for (i in seq_along(l_factor)) {
      expect_silent(shapr(train_df_used_factor, l_factor[[i]])) # No modification
      expect_message(shapr(train_df, l_factor[[i]])) # Features dropped
    }


    # Testing errors on incompatible model and data
    # Missing features
    model <- stats::lm(formula_factor, data = train_df)
    data_error <- train_df[, -3]
    expect_error(shapr(data_error, model))

    # Duplicated column names
    data_error <- train_df_used_factor
    data_error <- cbind(data_error, lstat = 1)
    expect_error(shapr(data_error, model))

    # Empty column names in data
    data_error <- train_df
    colnames(data_error) <- NULL
    expect_error(shapr(data_error, model))

    # Empty column names in model (ok if found in data -- and we trust it)
    if (requireNamespace("xgboost", quietly = TRUE)) {
      data_with_colnames <- data_without_colnames <- as.matrix(train_df_used_numeric)
      colnames(data_without_colnames) <- NULL

      model_xgb <- xgboost::xgboost(
        data = data_without_colnames, label = y_train,
        nrounds = 3, verbose = FALSE
      )
      expect_message(shapr(data_with_colnames, model_xgb))
    }

    # Data feature with incorrect class
    data_error <- train_df_used_factor
    data_error$lstat <- as.logical(data_error$lstat > 15)
    expect_error(shapr(data_error, model))

    # non-matching factor levels
    data_error <- head(train_df_used_factor)
    data_error$rad <- droplevels(data_error$rad)
    expect_error(shapr(data_error, model))
  }
})
