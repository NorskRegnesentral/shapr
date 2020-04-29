library(shapr)

context("test-src_weighted_matrix.R")

test_that("Test weight_matrix_cpp", {

  ## Example -----------
  m <- 3
  n <- 2^m
  features <- unlist(
    lapply(
      0:m,
      utils::combn,
      x = m,
      simplify = FALSE
    ),
    recursive = FALSE
  )
  w_all <- shapley_weights(m, choose(m, 0:m), 0:m)
  w_all[!is.finite(w_all)] <- 10^6
  w <- w_all[sapply(features, length) + 1]
  x <- weight_matrix_cpp(
    features = features,
    m = m,
    n = n,
    w = w
  )

  ## Exact results -----------
  Z <- matrix(0, nrow = n, ncol = m + 1)
  Z[, 1] <- 1
  for (i in seq_along(features)) {
    f <- features[[i]]
    if (length(f) > 0) {
      Z[i, f + 1] <- 1
    }
  }
  W <- matrix(0, nrow = n, ncol = n)
  diag(W) <- w
  res <- solve(t(Z) %*% W %*% Z) %*% (t(Z) %*% W)

  ## Test results -----------
  expect_true(is.matrix(x))
  expect_true(is.double(x))
  expect_equal(nrow(x), m + 1)
  expect_equal(ncol(x), n)
  expect_equal(x, res)
})

test_that("Test feature_matrix_cpp", {

  ## Example -----------
  features <- list(
    integer(0),
    1:2,
    10,
    4:8,
    3:7
  )
  m <- 10
  x <- feature_matrix_cpp(features, m)

  ## Test results -----------
  expect_true(is.matrix(x))
  expect_equal(ncol(x), m)
  expect_equal(nrow(x), length(features))
  expect_true(max(x) <= 1)
  expect_true(min(x) >= 0)
  expect_equal(sapply(features, length), rowSums(x))
  for (i in seq_along(features)) {
    feature_i <- features[[i]]
    n_features <- length(feature_i)
    if (n_features == 0) {
      expect_equal(x[i, ], rep(0, m))

    } else {
      expect_equal(x[i, feature_i], rep(1, n_features))
      expect_equal(x[i, -feature_i], rep(0, m - n_features))
    }
  }
  expect_error(feature_matrix_cpp(list(1, 2:3), 3))
})
