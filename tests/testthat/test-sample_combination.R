library(shapr)

context("test-sample_combinations.R")

test_that("Test function on extra.R", {

  # Example -----------
  nTrain <- 10
  nTest <- 10
  nosamp <- 7
  separate <- TRUE
  cnms <- c("samp_train", "samp_test")

  x <- sample_combinations(nTrain, nTest, nosamp, separate)


  # Tests -----------
  expect_true(is.data.frame(x))
  expect_equal(names(x), cnms)
  expect_equal(nrow(x), nosamp)

  # Expect all unique values when nosamp < nTrain
  expect_true(length(unique(x$samp_train)) == nosamp)
  expect_true(length(unique(x$samp_test)) == nosamp)

  expect_true(max(x$samp_train) <= nTrain)
  expect_true(max(x$samp_test) <= nTest)

  # Example -----------
  nTrain <- 5
  nTest <- 5
  nosamp <- 7
  separate <- TRUE

  x <- sample_combinations(nTrain, nTest, nosamp, separate)

  # Tests -----------
  expect_true(max(x$samp_train) <= nTrain)
  expect_true(max(x$samp_test) <= nTest)
  expect_equal(nrow(x), nosamp)

  # Example -----------
  nTrain <- 5
  nTest <- 5
  nosamp <- 7
  separate <- FALSE

  x <- sample_combinations(nTrain, nTest, nosamp, separate)

  # Tests -----------
  expect_true(max(x$samp_train) <= nTrain)
  expect_true(max(x$samp_test) <= nTest)
  expect_equal(nrow(x), nosamp)


})
