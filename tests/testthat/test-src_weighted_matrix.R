library(shapr)

context("test-src_weighted_matrix.R")

test_that("Test weight_matrix_cpp", {

  ## Example -----------
  features <- list(
    integer(0),
    1:2,
    10,
    4:8,
    3:7
  )
  nfeatures <- 10
  x <- feature_matrix_cpp(features, nfeatures)

  ## Test results -----------
  expect_true(is.matrix(x))
  expect_equal(ncol(x), nfeatures)
  expect_equal(nrow(x), length(features))
  expect_true(max(x) <= 1)
  expect_true(min(x) >= 0)
  expect_equal(sapply(features, length), rowSums(x))
  for (i in seq_along(features)) {
    feature_i <- features[[i]]
    num_features <- length(feature_i)
    if (num_features == 0) {
      expect_equal(x[i, ], rep(0, nfeatures))

    } else {
      expect_equal(x[i, feature_i], rep(1, num_features))
      expect_equal(x[i, -feature_i], rep(0, nfeatures - num_features))
    }
  }
  expect_error(feature_matrix_cpp(list(1, 2:3), 3))
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
  nfeatures <- 10
  x <- feature_matrix_cpp(features, nfeatures)

  ## Test results -----------
  expect_true(is.matrix(x))
  expect_equal(ncol(x), nfeatures)
  expect_equal(nrow(x), length(features))
  expect_true(max(x) <= 1)
  expect_true(min(x) >= 0)
  expect_equal(sapply(features, length), rowSums(x))
  for (i in seq_along(features)) {
    feature_i <- features[[i]]
    num_features <- length(feature_i)
    if (num_features == 0) {
      expect_equal(x[i, ], rep(0, nfeatures))

    } else {
      expect_equal(x[i, feature_i], rep(1, num_features))
      expect_equal(x[i, -feature_i], rep(0, nfeatures - num_features))
    }
  }
  expect_error(feature_matrix_cpp(list(1, 2:3), 3))
})
