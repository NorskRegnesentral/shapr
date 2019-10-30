library(shapr)

context("test-sample_combinations.R")

test_that("Test sample_combinations", {

  # Example -----------
  ntrain <- 10
  ntest <- 10
  nsamples <- 7
  joint_sampling <- FALSE
  cnms <- c("samp_train", "samp_test")

  set.seed(123) # Ensuring consistency in every test
  x <- sample_combinations(ntrain, ntest, nsamples, joint_sampling)

  # Tests -----------
  expect_true(is.data.frame(x))
  expect_equal(names(x), cnms)
  expect_equal(nrow(x), nsamples)

  # Expect all unique values when nsamples < ntrain
  expect_true(length(unique(x$samp_train)) == nsamples)
  expect_true(length(unique(x$samp_test)) == nsamples)

  expect_true(max(x$samp_train) <= ntrain)
  expect_true(max(x$samp_test) <= ntest)

  # Example -----------
  ntrain <- 5
  ntest <- 5
  nsamples <- 7
  joint_sampling <- FALSE

  x <- sample_combinations(ntrain, ntest, nsamples, joint_sampling)

  # Tests -----------
  expect_true(max(x$samp_train) <= ntrain)
  expect_true(max(x$samp_test) <= ntest)
  expect_equal(nrow(x), nsamples)

  # Example -----------
  ntrain <- 5
  ntest <- 5
  nsamples <- 7
  joint_sampling <- TRUE

  x <- sample_combinations(ntrain, ntest, nsamples, joint_sampling)

  # Tests -----------
  expect_true(max(x$samp_train) <= ntrain)
  expect_true(max(x$samp_test) <= ntest)
  expect_equal(nrow(x), nsamples)
})

test_that("test sample_gaussian", {
  # Example 1 -----------
  # Check that the given features are not resampled, but kept as is.
  m <- 10
  n_samples <- 50
  mu <- rep(1, m)
  set.seed(123) # Setting seed to avoid sampling a cov_mat which is exactly symmetric in certain dimensions,
                # such that ensure_condcov_symmetry would not affect the results
  cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
  x_test <- MASS::mvrnorm(2, mu, cov_mat)[1,,drop=F] # sample_gaussian requires x_test to be a matrix
  index_given <- 4
  set.seed(1)
  ret <- sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test)
  X_given <- x_test[index_given]
  res1.1 <- as.data.table(matrix(rep(X_given, each = n_samples), byrow = T))
  res1.2 <- as.data.table(ret[, ..index_given])
  colnames(res1.1) <- colnames(res1.2)

  # Example 2 -------------
  # Check that conditioning upon all variables simply returns the test observation.
  index_given <- 1:m
  x2 <- as.data.table(matrix(x_test, ncol = m, nrow = 1))
  res2 <- sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test)

  # Example 3 -------------
  # Check that ensuring conditional covariance matrix symmetry is FALSE by default.
  index_given <- 4:7
  set.seed(1)
  res3.1 <- sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = F)
  set.seed(1)
  res3.2 <- sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test)
  set.seed(1)
  res3.3 <- sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = T)

  # Tests ------------------
  expect_equal(res1.1, res1.2)
  expect_equal(x2, res2)
  expect_identical(res3.1, res3.2)
  expect_false(sum(res3.1 != res3.3) == 0) # Expect different results
  expect_error(sample_gaussian(m + 1, n_samples, mu, cov_mat, m, x_test))
  expect_true(data.table::is.data.table(res3.2))
})

test_that("test sample_copula", {
  # Example 1 --------------
  # Check that the given features are not resampled, but kept as is.
  m <- 10
  n <- 40
  n_samples <- 50
  mu <- rep(1, m)
  set.seed(123) # Ensuring consistency in every test
  cov_mat <- cov(matrix(rnorm(n * m), n, m))
  x_train <- MASS::mvrnorm(n, mu, cov_mat)
  x_test <- MASS::mvrnorm(1, mu, cov_mat)
  x_test_gaussian <- MASS::mvrnorm(1, mu, cov_mat)
  index_given <- 3:6
  set.seed(1)
  ret <- sample_copula(index_given, n_samples, mu, cov_mat, m, x_test_gaussian, x_train, x_test)
  X_given <- x_test[index_given]
  res1.1 <- as.data.table(matrix(rep(X_given, each = n_samples), nrow = n_samples))
  res1.2 <- as.data.table(ret[, ..index_given])
  colnames(res1.1) <- colnames(res1.2)

  # Example 2 --------------
  # Check that conditioning upon all variables simply returns the test observation.
  index_given <- 1:m
  x2 <- as.data.table(matrix(x_test, ncol = m, nrow = 1))
  res2 <- sample_copula(index_given, n_samples, mu, cov_mat, m, x_test_gaussian, x_train, x_test)

  # Example 3 --------------
  # Check that the colnames are preserved.
  index_given <- c(1, 2, 3, 5, 6)
  x_test <- t(as.data.frame(x_test))
  colnames(x_test) <- 1:m
  res3 <- sample_copula(index_given, n_samples, mu, cov_mat, m, x_test_gaussian, x_train, x_test)

  # Tests ------------------
  expect_equal(res1.1, res1.2)
  expect_equal(x2, res2)
  expect_identical(colnames(res3), colnames(x_test))
  expect_error(sample_copula(m + 1, n_samples, mu, cov_mat, m, x_test_gaussian, x_train, x_test))
  expect_true(data.table::is.data.table(res2))
})
