library(shapr)

context("test-features.R")

test_that("Test feature_combinations", {

  ## Example -----------
  x <- 1

  ## Test results -----------
  expect_equal(x - 1, 0)
  expect_error(x - "a")
})

test_that("Test feature_exact", {

  ## Example -----------
  m <- 3
  weight_zero_m <- 10^6
  x <- feature_exact(m, weight_zero_m)

  ## Define results -----------
  cnms <- c("ID", "features", "nfeatures", "N", "shapley_weight", "no")
  classes <- c("integer", "list", "integer", "integer", "double", "double")
  lfeatures <- list(
    integer(0),
    1L,
    2L,
    3L,
    c(1L, 2L),
    c(1L, 3L),
    c(2L, 3L),
    c(1L, 2L, 3L)
  )
  nfeatures <- c(0, rep(1, 3), rep(2, 3), 3)
  n <- c(1, rep(3, 6), 1)

  ## Tests -----------
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(unname(sapply(x, typeof)), classes)
  expect_equal(x[["ID"]], seq(nrow(x)))
  expect_equal(x[["features"]], lfeatures)
  expect_equal(x[["nfeatures"]], nfeatures)
  expect_equal(x[["N"]], n)
  expect_equal(x[["no"]], rep(1, nrow(x)))
})

test_that("Test feature_not_exact", {

  ## Example -----------
  x <- 1

  ## Test results -----------
  expect_equal(x - 1, 0)
  expect_error(x - "a")
})
