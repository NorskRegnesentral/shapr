library(shapr)

context("test-observations.R")

test_that("Test observation_impute", {

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

})
