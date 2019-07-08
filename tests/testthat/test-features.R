library(shapr)

context("test-features.R")

test_that("Test feature_combinations", {

  # Example 1 -----------
  m <- 3
  exact <- TRUE
  w <- 10^6
  x1 <- feature_combinations(m = m, exact = exact, weight_zero_m = w)
  x2 <- feature_exact(m, w)

  # Example 2 -----------
  m <- 10
  exact <- FALSE
  noSamp <- 50
  w <- 10^6
  reduce_dim <- TRUE
  set.seed(1)
  y1 <- feature_combinations(
    m = m,
    exact = exact,
    noSamp = noSamp,
    weight_zero_m = w,
    reduce_dim = reduce_dim
  )
  set.seed(1)
  y2 <-  feature_not_exact(
    m = m,
    noSamp = noSamp,
    weight_zero_m = w,
    reduce_dim = reduce_dim
  )

  # Example 3 -----------
  m <- 3
  exact <- FALSE
  noSamp <- 1e4
  w <- 10^6
  reduce_dim <- TRUE
  set.seed(1)
  y3 <- feature_combinations(
    m = m,
    exact = exact,
    noSamp = noSamp,
    weight_zero_m = w,
    reduce_dim = FALSE
  )

  # Test results -----------
  expect_equal(x1, x2)
  expect_equal(y1, y2)
  expect_equal(nrow(y3), 2^3)
})

test_that("Test feature_exact", {

  # Example -----------
  m <- 3
  weight_zero_m <- 10^6
  x <- feature_exact(m, weight_zero_m)

  # Define results -----------
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

  # Tests -----------
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

  # Example -----------
  m <- 10
  exact <- FALSE
  noSamp <- 50
  w <- 10^6
  reduce_dim <- FALSE
  set.seed(1)
  x <- feature_not_exact(
    m = m,
    noSamp = noSamp,
    weight_zero_m = w,
    reduce_dim = reduce_dim
  )
  set.seed(1)

  cnms <- c("ID", "features", "nfeatures", "N", "shapley_weight", "no")
  classes <- c("integer", "list", "integer", "integer", "double", "integer")

  # Test results -----------
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(unname(sapply(x, typeof)), classes)
  # expect_equal(x[["ID"]], seq(nrow(x)))
  # expect_equal(x[["features"]], lfeatures)
  # expect_equal(x[["nfeatures"]], nfeatures)
  # expect_equal(x[["N"]], n)
  # expect_equal(x[["no"]], rep(1, nrow(x)))

})

test_that("Test helper_feature", {

  # Example -----------
  m <- 5
  feature_sample <- list(
    integer(0),
    1:2,
    3:5,
    1:2,
    1:5
  )
  x <- helper_feature(m, feature_sample)

  # Define results -----------
  x1 <- c(0, 2, 3, 2, 5)
  x2 <- c(1, 2, 1, 2, 1)
  x3 <- c(FALSE, FALSE, FALSE, TRUE, FALSE)

  # Test results -----------
  cnms <- c("nfeatures", "no", "is_duplicate")
  classes <- c("integer", "integer", "logical")
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(nrow(x), length(feature_sample))
  expect_equal(classes, unname(sapply(x, typeof)))
  expect_equal(x[["nfeatures"]], x1)
  expect_equal(x[["no"]], x2)
  expect_equal(x[["is_duplicate"]], x3)

})
