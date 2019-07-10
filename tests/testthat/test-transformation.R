library(shapr)

context("test-transformation.R")

test_that("Test inv_gaussian_transform", {

  # Example -----------
  zx <- seq(-1, 1, length.out = 100)
  n_z <- 30
  type <- 7

  x <- inv_gaussian_transform(zx, n_z, type)

  # Tests -----------
  expect_true(is.vector(x))
  expect_true(is.double(x))

  expect_equal(length(x), n_z)
  expect_true(min(x) >= min(zx[-c(1:n_z)]))
  expect_true(max(x) <= max(zx[-c(1:n_z)]))

  expect_error(inv_gaussian_transform(zx, length(zx) + 1))

})
