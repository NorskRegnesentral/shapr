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


test_that("Test make_dummies", {

  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  x_train <- as.data.frame(Boston[401:411, x_var])
  y_train <- Boston[401:408, y_var]
  x_test <- as.data.frame(Boston[1:4, x_var])

  # convert to factors for illustational purpose
  x_train$rm <- factor(round(x_train$rm))
  x_test$rm <- factor(round(x_test$rm), levels = levels(x_train$rm))

  factor_feat <- sapply(x_train, is.factor)
  nb_factor_feat <- sum(factor_feat)

  dummylist <- make_dummies(data = rbind(x_train, x_test), newdata = x_train)

  # Tests
  expect_type(dummylist, "list")

  expect_equal(length(dummylist$obj$contrasts_list), nb_factor_feat)

  expect_equal(length(dummylist$obj$features), ncol(x_train))

  expect_equal(length(dummylist$obj$factor_features), nb_factor_feat)

  expect_equal(ncol(dummylist$obj$contrasts_list$rm), length(levels(x_train$rm)))

  # What if you have two variables with the same name? - error
  x_train2 <- x_train
  colnames(x_train2) <- c("X1", "X2", "X3", "X3")
  expect_error(make_dummies(data = x_train2, newdata = x_train))

  # What if one variables has an empty name? - error
  x_train3 <- x_train
  colnames(x_train3) <- c("", "X2", "X3", "X4")
  expect_error(make_dummies(data = x_train3, newdata = x_train3))

  # What if data has no column names - error
  x_train4 <- x_train
  colnames(x_train4) <- NULL
  expect_error(make_dummies(data = x_train4, newdata = x_train4))

  # What if data and newdata don't have the same levels?
  x_test_diff_levels = droplevels(x_test)

  expect_error(make_dummies(data = rbind(x_train, x_test_diff_levels), newdata = x_test_diff_levels))
  expect_error(make_dummies(data = rbind(x_train, x_test), newdata = x_test_diff_levels))

})

test_that("Test apply_dummies", {

  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  x_train <- as.data.frame(Boston[401:411, x_var])
  y_train <- Boston[401:408, y_var]
  x_test <- as.data.frame(Boston[1:4, x_var])

  # convert to factors for illustational purpose
  x_train$rm <- factor(round(x_train$rm))
  x_test$rm <- factor(round(x_test$rm), levels = levels(x_train$rm))

  numeric_feat <- !sapply(x_train, is.factor)
  nb_numeric_feat <- sum(numeric_feat)

  dummylist <- make_dummies(data = rbind(x_train, x_test), newdata = x_train)

  x_train_dummies <- apply_dummies(obj = dummylist$obj, newdata = x_train)

  # Tests
  expect_type(x_train_dummies, "double")

  expect_equal(ncol(x_train_dummies),
               nb_numeric_feat + length(dummylist$obj$factor_list$rm))

  # What if you re-arrange the columns in x_train?

  x_train0 <- x_train[, c(2, 3, 1, 4)]
  # apply_dummies will re-arrange the columns to match x_train in dummylist
  diff_column_placements <- apply_dummies(dummylist$obj, newdata = x_train0)
  expect_equal(colnames(diff_column_placements), colnames(x_train_dummies))

  # What if you put in less features then the original feature vector?
  x_train1 <- x_train[, c(2, 1)]
  expect_error(apply_dummies(dummylist$obj, newdata = x_train1))

  # What if you change the feature types?
  x_train_num <- sapply(x_train, as.numeric)
  expect_error(apply_dummies(dummylist$obj, newdata = x_train_num))

  # What if you add a feature?
  x_train2 <- cbind(x_train[, c(1, 2)], new_var = x_train[, 2], x_train[, c(3, 4)])

  # will not throw an error - do we want it to throw an error?
  a_new_var <- apply_dummies(dummylist$obj, newdata = x_train2)
  expect_equal(ncol(a_new_var), ncol(x_train_dummies))

})
