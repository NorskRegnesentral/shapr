library(shapr)

context("test-transformation.R")

test_that("Test inv_gaussian_transform", {

  # Example -----------
  zx <- rnorm(50)
  n_z <- 30
  type <- 7

  x <- inv_gaussian_transform(zx, n_z, type)

  # Tests -----------
  expect_true(is.atomic(x))
  expect_true(is.double(x))

  expect_equal(length(x), n_z)
  expect_true(min(x) >= min(zx[-c(1:n_z)]))
  expect_true(max(x) <= max(zx[-c(1:n_z)]))

})

test_that("Test gaussian_transform_separate", {

  # Example -----------
  yx <- rnorm(50)
  n_y <- 30

  x <- gaussian_transform_separate(yx, n_y)

  # Tests -----------
  expect_true(is.atomic(x))
  expect_true(is.double(x))
  expect_equal(length(x), n_y)

})

test_that("Test gaussian_transform", {

  # Example -----------
  y <- rnorm(50)
  x <- gaussian_transform(y)

  # Tests -----------
  expect_true(is.atomic(x))
  expect_true(is.double(x))
  expect_equal(length(x), length(y))


})
