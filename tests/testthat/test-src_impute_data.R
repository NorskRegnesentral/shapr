library(shapr)

context("test-src_impute_data.R")

test_that("Test observation_impute_cpp", {

  # Example data -----------
  data("mtcars")
  rownames(mtcars) <- NULL
  mtcars <- as.matrix(mtcars)

  # Example -----------
  m <- 3
  n_combinations <- 2^m
  mtcars <- mtcars[1:15, seq(m)]
  ntrain <- 14
  xtrain <- mtcars[seq(ntrain), ]
  xtest <- mtcars[-seq(ntrain), , drop = FALSE]
  S <- matrix(0L, n_combinations, m)
  features <- list(
    integer(), 1, 2, 3, c(1, 2), c(1, 3), c(2, 3), c(1, 2, 3)
  )
  for (i in seq_along(features)) {
    feature_i <- features[[i]]
    if (length(feature_i) > 0) {
      S[i, features[[i]]] <- 1L
    }
  }

  # Tests (invalid input) -----------
  expect_error(
    observation_impute_cpp(
      index_xtrain = c(1, 2),
      index_s = c(1, 2, 3),
      xtrain = xtrain,
      xtest = xtest,
      S = S
    )
  )
  expect_error(
    observation_impute_cpp(
      index_xtrain = c(1, 2),
      index_s = c(2, 3),
      xtrain = xtrain[, 1:2],
      xtest = xtest,
      S = S
    )
  )

  # Tests (valid input) -----------
  index_xtrain <- c(1, 2)
  index_s <- c(4, 5)
  x <- observation_impute_cpp(
    index_xtrain = index_xtrain,
    index_s = index_s,
    xtrain = xtrain,
    xtest = xtest,
    S = S
  )

  expect_equal(nrow(x), length(index_s))
  expect_equal(ncol(x), ncol(xtrain))
  expect_true(is.matrix(x))
  expect_true(is.double(x))

  for (i in 1:nrow(x)) {

    feature_i <- features[[index_s[i]]]

    for (j in seq(m)) {

      if (j %in% feature_i) {
        expect_equal(x[i, j], unname(xtest[1, j]))
      }else {
        expect_equal(x[i, j], unname(xtrain[index_xtrain[i], j]))
      }
    }
  }
})
