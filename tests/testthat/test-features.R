library(shapr)
library(testthat)

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
  n_combinations <- 50
  w <- 10^6

  set.seed(1)
  y1 <- feature_combinations(
    m = m,
    exact = exact,
    n_combinations = n_combinations,
    weight_zero_m = w)

  set.seed(1)
  y2 <- feature_not_exact(
    m = m,
    n_combinations = n_combinations,
    weight_zero_m = w
  )
  y2[, p := NULL]

  # Example 3 -----------
  m <- 3
  exact <- FALSE
  n_combinations <- 1e4
  w <- 10^6
  set.seed(1)
  y3 <- feature_combinations(
    m = m,
    exact = exact,
    n_combinations = n_combinations,
    weight_zero_m = w
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
  weight_zero_m <- 10^6
  x <- feature_exact(m, weight_zero_m)

  # Define results -----------
  cnms <- c("id_combination", "features", "n_features", "N", "shapley_weight")
  classes <- c("integer", "list", "integer", "integer", "double")
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
  n_features <- c(0, rep(1, 3), rep(2, 3), 3)
  n <- c(1, rep(3, 6), 1)

  # Tests -----------
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(unname(sapply(x, typeof)), classes)
  expect_equal(x[["id_combination"]], seq(nrow(x)))
  expect_equal(x[["features"]], lfeatures)
  expect_equal(x[["n_features"]], n_features)
  expect_equal(x[["N"]], n)
})

test_that("Test feature_not_exact", {

  # Example -----------
  m <- 10
  exact <- FALSE
  n_combinations <- 50
  w <- 10^6
  set.seed(1)
  x <- feature_not_exact(
    m = m,
    n_combinations = n_combinations,
    weight_zero_m = w
  )
  set.seed(1)

  cnms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
  classes <- c("integer", "list", "integer", "integer", "integer", "double")
  n <- sapply(seq(m - 1), choose, n = m)
  w_all <- shapley_weights(m = m, N = n, n_features = seq(m - 1)) * n
  w_default <- w_all / sum(w_all)

  # Test results -----------
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(unname(sapply(x, typeof)), classes)
  expect_true(nrow(x) <= n_combinations + 2)
  expect_equal(x[["id_combination"]], seq(nrow(x)))
  for (i in x[, .I]) {
    f <- x[["features"]][[i]]
    if (length(f) == 0) {
      expect_equal(x[["n_features"]][[i]], 0)
      expect_equal(x[["N"]][[i]], 1)
      expect_equal(x[["shapley_weight"]][[i]], w)
      expect_equal(x[["p"]][[i]], NA_real_)

    } else if (length(f) == m) {
      expect_equal(f, seq(m))
      expect_equal(x[["n_features"]][[i]], m)
      expect_equal(x[["N"]][[i]], 1)
      expect_equal(x[["shapley_weight"]][[i]], w)
      expect_equal(x[["p"]][[i]], NA_real_)

    } else {
      k <- length(f)
      expect_equal(f, sort(f))
      expect_equal(x[["n_features"]][[i]], k)
      expect_equal(x[["N"]][[i]], choose(m, k))
      expect_equal(x[["p"]][[i]], w_default[x[["n_features"]][[i]]])
      expect_equal(between(x[["shapley_weight"]][[i]], 1L, n_combinations), TRUE)
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
  cnms <- c("sample_frequence", "is_duplicate")
  classes <- c("integer", "logical")
  expect_true(data.table::is.data.table(x))
  expect_equal(names(x), cnms)
  expect_equal(nrow(x), length(feature_sample))
  expect_equal(classes, unname(sapply(x, typeof)))
  expect_equal(x[["sample_frequence"]], x2)
  expect_equal(x[["is_duplicate"]], x3)
})

test_that("Test feature_group", {

  ## 1
  group1_num <- list(c(1), c(2), c(3), c(4))
  expect_silent(feature_group(group1_num, weight_zero_m = 10^6))
  expect_silent(dim(feature_group(group1_num, weight_zero_m = 10^6))[1] == 16)

  ## 2
  group2_num <- list(c(1, 2), c(3, 4))
  expect_silent(feature_group(group2_num, weight_zero_m = 10^6))
  expect_silent(dim(feature_group(group2_num, weight_zero_m = 10^6))[1] == 2)

  # Empty call
  expect_error(feature_group())

})

test_that("Test check_group", {

  x_var <- c("lstat", "rm","dis",
             "indus","nox",
             "tax")

  group1_num <- list(c(1,2,3),
                     c(4,5),
                     c(6))

  group1_names = lapply(group1_num, function(x){x_var[x]})

  # Indendend usage
  expect_silent(check_groups(x_var, group1_names, FALSE))
  expect_silent(check_groups(x_var, group1_names, TRUE))

  group2_num <- list(c(1,2,3),
                     c(4,1),
                     c(6))

  # Repeated group name
  group2_names = lapply(group2_num, function(x){x_var[x]})
  expect_error(check_groups(x_var, group2_names, FALSE))

  group3_names = group1_names
  group3_names[[3]][2] = "not_in_feature_labels"

  # feature in group not in feature_labels
  expect_error(check_groups(x_var, group3_names, FALSE))
  expect_error(check_groups(x_var, group3_names, TRUE))

  group4_names = group1_names
  group4_names[[3]] = c(1,2)

  # non-character group
  expect_error(check_groups(x_var, group4_names, FALSE))
  expect_error(check_groups(x_var, group4_names, TRUE))

})
