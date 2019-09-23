library(shapr)

context("test-observations.R")

test_that("Test observation_impute", {
  # Example 1 -----------------
  # Test the default argument n_samples
  n <- 20
  m <- 2
  sigma <- cov(matrix(MASS::mvrnorm(m * n, 0, 1), nrow = n))
  x_train <- as.matrix(MASS::mvrnorm(n, mu = rep(0, m), Sigma = sigma), ncol = m)
  x_test <- t(as.matrix(MASS::mvrnorm(1, mu = rep(0, m), sigma)))
  colnames(x_train) <- colnames(x_test) <- c("X1", "X2")
  Ytrain <- rnorm(n, 0, 2)
  S <- matrix(c(1, 0, 0, 1), nrow = 2)
  W_kernel <- matrix(rnorm(n * ncol(S), mean = 1 / n, sd = 1 / n^2), nrow = n)
  res1_1 <- observation_impute(W_kernel, S, x_train, x_test, w_threshold = .7, n_samples = 1e3)
  res1_2 <- observation_impute(W_kernel, S, x_train, x_test, w_threshold = .7)

  # Example 2 ------------------
  # Test the default argument w_threshold
  res2_1 <- observation_impute(W_kernel, S, x_train, x_test, w_threshold = .7, n_samples = 1e3)
  res2_2 <- observation_impute(W_kernel, S, x_train, x_test, n_samples = 1e3)

  # Example 3 ------------------
  # Test that w_threshold can be adjusted by alternative arguments, changing the dimension of the results.
  res3_1 <- observation_impute(W_kernel, S, x_train, x_test, w_threshold = .7)
  res3_2 <- observation_impute(W_kernel, S, x_train, x_test, w_threshold = 0.5)

  # Tests ----------------------
  expect_equal(res1_1, res1_2)
  expect_equal(res2_1, res2_2)
  expect_true(all(sapply(colnames(x_train), function(s) s %in% colnames(res2_1))))
  expect_false(all(dim(res3_1) == dim(res3_2)))
  expect_error(observation_impute(W_kernel = 1, S, x_train, x_test))
})
