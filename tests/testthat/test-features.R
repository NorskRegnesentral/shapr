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
    weight_zero_m = w
  )

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
  if (requireNamespace("MASS", quietly = TRUE)) {
    data("Boston", package = "MASS")
    x_var <- c("lstat", "rm", "dis", "indus")
    y_var <- "medv"

    x_train <- as.data.frame(Boston[401:411, x_var])
    y_train <- Boston[401:408, y_var]
    x_test <- as.data.frame(Boston[1:4, x_var])

    # convert to factors for illustrational purpose
    x_train$rm <- factor(round(x_train$rm))
    x_test$rm <- factor(round(x_test$rm), levels = levels(x_train$rm))

    factor_feat <- sapply(x_train, is.factor)
    nb_factor_feat <- sum(factor_feat)

    dummylist <- make_dummies(traindata = x_train, testdata = x_train)

    # Tests
    expect_type(dummylist, "list")

    expect_equal(length(dummylist$feature_list$contrasts_list), nb_factor_feat)

    expect_equal(length(dummylist$feature_list$labels), ncol(x_train))

    expect_equal(sum(dummylist$feature_list$classes == "factor"), nb_factor_feat)

    expect_equal(ncol(dummylist$feature_list$contrasts_list$rm), length(levels(x_train$rm)))

    # 1) What if train has more features than test but features in test are contained in train
    x_train1 <- cbind(x_train, 1)
    x_test1 <- x_test
    expect_error(make_dummies(traindata = x_train1, testdata = x_test1))

    # 2) What if train has different feature types than test
    x_train2 <- x_train
    x_test2 <- x_test
    x_test2$rm <- as.numeric(x_test2$rm)
    expect_error(make_dummies(traindata = x_train2, testdata = x_test2))

    # 3) What if test has more features than train but features in train are contained in test
    x_train3 <- x_train
    x_test3 <- cbind(x_test, 1)
    expect_error(make_dummies(traindata = x_train3, testdata = x_test3))

    # 4) What if train and test only have numerical features
    x_train4 <- x_train
    x_train4$rm <- as.numeric(x_train4$rm)
    x_test4 <- x_test
    x_test4$rm <- as.numeric(x_test4$rm)
    expect_type(make_dummies(traindata = x_train4, testdata = x_test4), "list")

    # 5) What if train and test only have categorical features
    x_train5 <- x_train
    x_train5 <- x_train5[, "rm", drop = FALSE]
    x_test5 <- x_test
    x_test5 <- x_test5[, "rm", drop = FALSE]
    expect_type(make_dummies(traindata = x_train5, testdata = x_test5), "list")

    # 6) What if test has the same levels as train but random ordering of levels
    x_train6 <- x_train
    x_train6$rm <- factor(x_train6$rm, levels = 4:9)
    x_test6 <- x_test
    x_test6$rm <- factor(x_test6$rm, levels = c(8, 9, 7, 4, 5, 6))
    expect_type(make_dummies(traindata = x_train6, testdata = x_test6), "list")

    # 7) What if test has different levels than train
    x_train7 <- x_train
    x_train7$rm <- factor(x_train7$rm, levels = 4:9)
    x_test7 <- x_test
    x_test7$rm <- factor(x_test7$rm, levels = 6:8)
    expect_error(make_dummies(traindata = x_train7, testdata = x_test7))

    # 8) What if train and test have different feature names
    x_train8 <- x_train
    x_test8 <- x_test
    names(x_test8) <- c("lstat2", "rm2", "dis2", "indus2")
    expect_error(make_dummies(traindata = x_train8, testdata = x_test8))

    # 9) What if one variables has an empty name
    x_train9 <- x_train
    colnames(x_train9) <- c("", "rm", "dis", "indus")
    x_test9 <- x_test
    colnames(x_test9) <- c("", "rm", "dis", "indus")
    expect_error(make_dummies(traindata = x_train9, testdata = x_test9))

    # 10) What if traindata has a column that repeats
    x_train10 <- cbind(x_train, lstat = x_train$lstat)
    x_test10 <- cbind(x_test, lstat = x_test$lstat)
    expect_error(make_dummies(traindata = x_train10, testdata = x_test10))

    # 11) What if traindata has no column names
    x_train11 <- x_train
    colnames(x_train11) <- NULL
    x_test11 <- x_test
    colnames(x_test11) <- NULL
    expect_error(make_dummies(traindata = x_train11, testdata = x_test11))

    # 12 Test that traindata_new and testdata_new will be the same as the original
    # x_train and x_test. The only time this is different is if the levels of train
    # and test are different. See below.
    dummylist12 <- make_dummies(traindata = x_train, testdata = x_test)
    #
    expect_true(all(data.frame(dummylist12$traindata_new) == x_train))
    expect_true(all(levels(dummylist12$traindata_new$rm) == levels(x_train$rm)))
    expect_true(all(data.frame(dummylist12$testdata_new) == x_test))
    expect_true(all(levels(dummylist12$testdata_new$rm) == levels(x_test$rm)))


    # 13 Different levels same as check # 12
    #
    x_train13 <- x_train
    x_train13$rm <- factor(x_train13$rm, levels = 4:9)
    x_test13 <- x_test
    x_test13$rm <- factor(x_test13$rm, levels = c(8, 9, 7, 4, 5, 6))
    dummylist13 <- make_dummies(traindata = x_train13, testdata = x_test13)
    #
    expect_true(all(data.frame(dummylist13$traindata_new) == x_train13))
    expect_true(all(levels(dummylist13$traindata_new$rm) == levels(x_train13$rm)))
    expect_true(all(data.frame(dummylist13$testdata_new) == x_test13))
    # Important !!!!
    expect_false(all(levels(dummylist13$testdata_new$rm) == levels(x_test13$rm)))
  }
})

test_that("Test apply_dummies", {
  if (requireNamespace("MASS", quietly = TRUE)) {
    data("Boston", package = "MASS")
    x_var <- c("lstat", "rm", "dis", "indus")
    y_var <- "medv"

    x_train <- as.data.frame(Boston[401:411, x_var])
    y_train <- Boston[401:408, y_var]
    x_test <- as.data.frame(Boston[1:4, x_var])

    # convert to factors for illustrational purpose
    x_train$rm <- factor(round(x_train$rm))
    x_test$rm <- factor(round(x_test$rm), levels = levels(x_train$rm))

    numeric_feat <- !sapply(x_train, is.factor)
    nb_numeric_feat <- sum(numeric_feat)

    dummylist <- make_dummies(traindata = x_train, testdata = x_test)

    x_test_dummies <- apply_dummies(feature_list = dummylist$feature_list, testdata = x_test)

    # Tests
    expect_type(x_test_dummies, "double")

    expect_equal(
      ncol(x_test_dummies),
      nb_numeric_feat + ncol(dummylist$feature_list$contrasts_list$rm)
    )

    # Test that make_dummies() and apply_dummies() gives the same output
    # for a given traindata and testdata
    expect_true(all(dummylist$test_dummies == x_test_dummies))

    # 1) What if you re-arrange the columns in x_train
    x_test1 <- x_test[, c(2, 3, 1, 4)]
    diff_column_placements <- apply_dummies(dummylist$feature_list, testdata = x_test1)
    expect_equal(colnames(diff_column_placements), colnames(x_test_dummies))

    # 2) What if you put in less features then the original traindata
    x_test2 <- x_test[, c(2, 1)]
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test2))

    # 3) What if you change the feature types of testdata
    x_test3 <- sapply(x_test, as.numeric)
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test3))

    # 4) What if you add a feature
    x_test4 <- cbind(x_train[, c(1, 2)], new_var = x_train[, 2], x_train[, c(3, 4)])
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test4))

    # 6) What if test has the same levels as train but random ordering of levels
    x_test6 <- x_test
    x_test6$rm <- factor(x_test6$rm, levels = c(8, 9, 7, 4, 5, 6))
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test6))

    # 7) What if test has different levels than train
    x_test7 <- x_test
    x_test7$rm <- factor(x_test7$rm, levels = 6:8)
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test7))

    # 8) What if train and test have different feature names
    x_test8 <- x_test
    names(x_test8) <- c("lstat2", "rm2", "dis2", "indus2")
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test8))

    # 9) What if one variables has an empty name
    x_test9 <- x_test
    colnames(x_test9) <- c("", "rm", "dis", "indus")
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test9))

    # 10) What if traindata has a column that repeats
    x_test10 <- cbind(x_test, lstat = x_test$lstat)
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test10))

    # 11) What if testdata has no column names
    x_test11 <- x_test
    colnames(x_test11) <- NULL
    expect_error(apply_dummies(dummylist$feature_list, testdata = x_test11))
  }
})
