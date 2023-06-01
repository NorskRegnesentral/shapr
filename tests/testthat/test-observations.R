context("test-observations.R")

test_that("Test observation_impute", {
  if (requireNamespace("MASS", quietly = TRUE)) {
    # Examples
    n <- 20
    m <- 2
    sigma <- cov(matrix(MASS::mvrnorm(m * n, 0, 1), nrow = n))
    x_train <- as.matrix(MASS::mvrnorm(n, mu = rep(0, m), Sigma = sigma), ncol = m)
    x_test <- t(as.matrix(MASS::mvrnorm(1, mu = rep(0, m), sigma)))
    colnames(x_train) <- colnames(x_test) <- paste0("X", seq(m))
    S <- matrix(c(1, 0, 0, 1), nrow = m)
    W_kernel <- matrix(rnorm(n * ncol(S), mean = 1 / n, sd = 1 / n^2), nrow = n)
    r <- observation_impute(W_kernel, S, x_train, x_test)

    # Test the default argument n_samples
    expect_equal(
      observation_impute(W_kernel, S, x_train, x_test, n_samples = 1e3),
      observation_impute(W_kernel, S, x_train, x_test)
    )

    # Test the default argument w_threshold
    expect_equal(
      observation_impute(W_kernel, S, x_train, x_test, w_threshold = .7),
      observation_impute(W_kernel, S, x_train, x_test)
    )

    # Test that w_threshold reduces number of rows
    expect_true(
      nrow(observation_impute(W_kernel, S, x_train, x_test, w_threshold = .7)) >
        nrow(observation_impute(W_kernel, S, x_train, x_test, w_threshold = 0.5))
    )

    # Test that n_samples reduces number of rows
    expect_true(
      nrow(observation_impute(W_kernel, S, x_train, x_test)) >
        nrow(observation_impute(W_kernel, S, x_train, x_test, n_samples = 10))
    )

    # Tests error
    expect_error(observation_impute(1, S, x_train, x_test))
    expect_error(observation_impute(W_kernel, 1, x_train, x_test))
    expect_error(observation_impute(W_kernel, tail(S, -1), x_train, x_test))
    expect_error(observation_impute(tail(W_kernel, -1), S, x_train, x_test))

    # Test single result
    cnms <- c(colnames(x_train), "id_combination", "w")
    expect_true(data.table::is.data.table(r))
    expect_true(ncol(r) == m + 2)
    expect_true(all(colnames(r) == cnms))
    expect_true(all(unlist(lapply(r, is.numeric))))
    expect_true(is.integer(r$id_combination))
  }
})


test_that("Check correct index_feature usage in prepare_data", {

  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  y_train <- tail(Boston[, y_var], 50)
  x <- as.matrix(head(Boston[, x_var], 2))
  n_samples <- 100
  index_features <- 4:7
  w_threshold = 0.95
  type = "fixed_sigma"
  fixed_sigma_vec = 0.1
  n_samples_aicc = 1000
  eval_max_aicc = 20
  start_aicc = 0.1
  mincriterion = 0.95
  minsplit = 20
  minbucket = 7
  sample = TRUE

  explainer <- readRDS(file = "test_objects/shapley_explainer_obj.rds")
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$n_samples <- n_samples

  explainer$approach <- "independence"
  dt <- prepare_data(explainer, index_features = index_features)
  expect_identical(sort(dt[,unique(id_combination)]),index_features)

  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$n_samples_aicc <- n_samples_aicc
  explainer$eval_max_aicc <- eval_max_aicc
  explainer$start_aicc <- start_aicc
  explainer$w_threshold <- w_threshold
  explainer$cov_mat <- stats::cov(explainer$x_train)

  explainer$approach <- "empirical"
  dt <- prepare_data(explainer, index_features = index_features)
  expect_identical(sort(dt[,unique(id_combination)]),index_features)

  explainer$mu <- unname(colMeans(explainer$x_train))
  explainer$approach <- "gaussian"
  dt <- prepare_data(explainer, index_features = index_features)
  expect_identical(sort(dt[,unique(id_combination)]),index_features)

  explainer$x_test_gaussian <- explainer$x_test # Shortcut
  explainer$approach <- "copula"
  dt <- prepare_data(explainer, index_features = index_features)
  expect_identical(sort(dt[,unique(id_combination)]),index_features)

  explainer$x_test_gaussian <- explainer$x_test # Shortcut
  explainer$approach <- "copula"
  dt <- prepare_data(explainer, index_features = index_features)
  expect_identical(sort(dt[,unique(id_combination)]),index_features)

  explainer$mincriterion <- mincriterion
  explainer$minsplit <- minsplit
  explainer$minbucket <- minbucket
  explainer$sample <- sample
  explainer$approach <- "ctree"
  explainer$x_test <- preprocess_data(x, explainer$feature_list)$x_dt
  dt <- prepare_data(explainer, index_features = index_features)
  expect_identical(sort(dt[,unique(id_combination)]),index_features)

})
