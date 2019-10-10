library(shapr)

context("test-features.R")

test_that("Test feature_combinations", {

  # Example 1 -----------
  m <- 3
  exact <- TRUE
  w <- 10^6
  x1 <- feature_combinations(m = m, exact = exact, shapley_weight_inf_replacement = w)
  x2 <- feature_exact(m, w)

  # Example 2 -----------
  m <- 10
  exact <- FALSE
  n_combinations <- 50
  w <- 10^6
  reduce_dim <- TRUE
  set.seed(1)
  y1 <- feature_combinations(
    m = m,
    exact = exact,
    n_combinations = n_combinations,
    shapley_weight_inf_replacement = w,
    reduce_dim = reduce_dim
  )
  set.seed(1)
  y2 <- feature_not_exact(
    m = m,
    n_combinations = n_combinations,
    shapley_weight_inf_replacement = w,
    reduce_dim = reduce_dim
  )

  # Example 3 -----------
  m <- 3
  exact <- FALSE
  n_combinations <- 1e4
  w <- 10^6
  reduce_dim <- TRUE
  set.seed(1)
  y3 <- feature_combinations(
    m = m,
    exact = exact,
    n_combinations = n_combinations,
    shapley_weight_inf_replacement = w,
    reduce_dim = FALSE
  )

  # Test results -----------
  expect_equal(x1, x2)
  expect_equal(y1, y2)
  expect_equal(nrow(y3), 2^3)
  expect_error(feature_combinations(100))
  expect_error(feature_combinations(100, n_combinations = NULL))
})

test_that("Test feature_exact", {

  # Example -----------
  m <- 3
  shapley_weight_inf_replacement <- 10^6
  x <- feature_exact(m, shapley_weight_inf_replacement)

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
  n_combinations <- 50
  w <- 10^6
  reduce_dim <- FALSE
  set.seed(1)
  x <- feature_not_exact(
    m = m,
    n_combinations = n_combinations,
    shapley_weight_inf_replacement = w,
    reduce_dim = reduce_dim
  )
  set.seed(1)

  cnms <- c("ID", "features", "nfeatures", "N", "shapley_weight", "no")
  classes <- c("integer", "list", "integer", "integer", "double", "integer")
  n <- sapply(seq(m - 1), choose, n = m)
  w_all <- shapley_weights(m = m, N = n, s = seq(m - 1)) * n
  w_default <- w_all / sum(w_all)

  # Test results -----------
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(unname(sapply(x, typeof)), classes)
  expect_equal(nrow(x), n_combinations + 2)
  expect_equal(x[["ID"]], seq(nrow(x)))
  for (i in x[, .I]) {
    f <- x[["features"]][[i]]
    if (length(f) == 0) {
      expect_equal(x[["nfeatures"]][[i]], 0)
      expect_equal(x[["N"]][[i]], 1)
      expect_equal(x[["shapley_weight"]][[i]], w)
      expect_equal(x[["no"]][[i]], 1)

    } else if (length(f) == m) {
      expect_equal(f, seq(m))
      expect_equal(x[["nfeatures"]][[i]], m)
      expect_equal(x[["N"]][[i]], 1)
      expect_equal(x[["shapley_weight"]][[i]], w)
      expect_equal(x[["no"]][[i]], 1)

    } else {
      k <- length(f)
      expect_equal(f, sort(f))
      expect_equal(x[["nfeatures"]][[i]], k)
      expect_equal(x[["N"]][[i]], choose(m, k))
      expect_equal(x[["shapley_weight"]][[i]], w_default[x[["nfeatures"]][[i]]])
      # TODO: add test for no column
    }
  }
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
  x2 <- c(1, 2, 1, 2, 1)
  x3 <- c(FALSE, FALSE, FALSE, TRUE, FALSE)

  # Test results -----------
  cnms <- c("no", "is_duplicate")
  classes <- c("integer", "logical")
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(nrow(x), length(feature_sample))
  expect_equal(classes, unname(sapply(x, typeof)))
  expect_equal(x[["no"]], x2)
  expect_equal(x[["is_duplicate"]], x3)

})
