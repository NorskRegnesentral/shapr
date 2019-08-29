library(shapr)

context("test-observations.R")

test_that("Test observation_impute", {
  # Example 1 -----------------
  # Test the default argument noSamp_MC
  n <- 20
  m <- 2
  Sigma <- cov(matrix(MASS::mvrnorm(m * n, 0, 1), nrow = n))
  Xtrain <- as.matrix(MASS::mvrnorm(n, mu = rep(0, m), Sigma = Sigma), ncol = m)
  Xtest <- t(as.matrix(MASS::mvrnorm(1, mu = rep(0, m), Sigma)))
  colnames(Xtrain) <- colnames(Xtest) <- c("X1", "X2")
  Ytrain <- rnorm(n, 0, 2)
  S <- matrix(c(1, 0, 0, 1), nrow = 2)
  W_kernel <- matrix(rnorm(n * ncol(S), mean = 1 / n, sd = 1 / n^2), nrow = n)
  res1.1 <- observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3)
  res1.2 <- observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7)

  # Example 2 ------------------
  # Test the default argument w_threshold
  res2.1 <- observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3)
  res2.2 <- observation_impute(W_kernel, S, Xtrain, Xtest, noSamp_MC = 1e3)

  # Example 3 ------------------
  # Test that w_threshold can be adjusted by alternative arguments, changing the dimension of the results.
  res3.1 <- observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7)
  res3.2 <- observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = 0.5)

  # Tests ----------------------
  expect_equal(res1.1, res1.2)
  expect_equal(res2.1, res2.2)
  expect_true(all(sapply(colnames(Xtrain), function(s) s %in% colnames(res2.1))))
  expect_false(all(dim(res3.1) == dim(res3.2)))
  expect_error(observation_impute(W_kernel = 1, S, Xtrain, Xtest))
})
