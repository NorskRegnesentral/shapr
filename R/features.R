#' Define feature combinations, and fetch additional information about each unique combination
#'
#' @param m Positive integer. Total number of features.
#' @param exact Logical. If \code{TRUE} all \code{2^m} combinations are generated, otherwise a
#' subsample of the combinations is used.
#' @param n_combinations Positive integer. Note that if \code{exact = TRUE},
#' \code{n_combinations} is ignored. However, if \code{m > 12} you'll need to add a positive integer
#' value for \code{n_combinations}.
#' @param weight_zero_m Numeric. The value to use as a replacement for infinite combination
#' weights when doing numerical operations.
#'
#' @return A data.table that contains the following columns:
#' \describe{
#' \item{id_combination}{Positive integer. Represents a unique key for each combination. Note that the table
#' is sorted by \code{id_combination}, so that is always equal to \code{x[["id_combination"]] = 1:nrow(x)}.}
#' \item{features}{List. Each item of the list is an integer vector where \code{features[[i]]}
#' represents the indices of the features included in combination \code{i}. Note that all the items
#' are sorted such that \code{features[[i]] == sort(features[[i]])} is always true.}
#' \item{n_features}{Vector of positive integers. \code{n_features[i]} equals the number of features in combination
#' \code{i}, i.e. \code{n_features[i] = length(features[[i]])}.}.
#' \item{N}{Positive integer. The number of unique ways to sample \code{n_features[i]} features
#' from \code{m} different features, without replacement.}
#' }
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
#'
#' @examples
#' # All combinations
#' x <- feature_combinations(m = 3)
#' nrow(x) # Equals 2^3 = 8
#'
#' # Subsample of combinations
#' x <- feature_combinations(exact = FALSE, m = 10, n_combinations = 1e2)
feature_combinations <- function(m, exact = TRUE, n_combinations = 200, weight_zero_m = 10^6) {

  # Force user to use a natural number for n_combinations if m > 13
  if (m > 13 & is.null(n_combinations)) {
    stop(
      paste0(
        "Due to computational complexity, we recommend setting n_combinations = 10 000\n",
        "if the number of features is larger than 13. Note that you can force the use of the exact\n",
        "method (i.e. n_combinations = NULL) by setting n_combinations equal to 2^m,\n",
        "where m is the number of features."
      )
    )
  }

  # Not supported for m > 30
  if (m > 30) {
    stop("Currently we are not supporting cases where the number of features is greater than 30.")
  }

  if (!exact && n_combinations > (2^m - 2)) {
    n_combinations <- 2^m - 2
    exact <- TRUE
    message(
      paste0(
        "\nn_combinations is larger than or equal to 2^m = ", 2^m, ". \n",
        "Using exact instead."
        )
      )
  }

  if (exact) {
    dt <- feature_exact(m, weight_zero_m)
  } else {
    dt <- feature_not_exact(m, n_combinations, weight_zero_m)
    stopifnot(
      data.table::is.data.table(dt),
      !is.null(dt[["p"]])
    )
    p <- NULL # due to NSE notes in R CMD check
    dt[, p := NULL]
  }

  return(dt)
}

#' @keywords internal
feature_exact <- function(m, weight_zero_m = 10^6) {

  features <- id_combination <- n_features <- shapley_weight <- N <- NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(id_combination = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
  dt[, features := unlist(combinations, recursive = FALSE)]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_features]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_features, weight_zero_m)]

  return(dt)
}

#' @keywords internal
feature_not_exact <- function(m, n_combinations = 200, weight_zero_m = 10^6) {

  features <- id_combination <- n_features <- shapley_weight <- N <- NULL # due to NSE notes in R CMD check

  # Find weights for given number of features ----------
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # Sample number of chosen features ----------
  X <- data.table::data.table(
    n_features = c(
      0,
      sample(
        x = n_features,
        size = n_combinations,
        replace = TRUE,
        prob = p
      ),
      m
    )
  )
  X[, n_features := as.integer(n_features)]

  # Sample specific set of features -------
  data.table::setkeyv(X, "n_features")
  feature_sample <- sample_features_cpp(m, X[["n_features"]])

  # Get number of occurences and duplicated rows-------
  is_duplicate <- NULL # due to NSE notes in R CMD check
  r <- helper_feature(m, feature_sample)
  X[, is_duplicate := r[["is_duplicate"]]]

  # When we sample combinations the Shapley weight is equal
  # to the frequency of the given combination
  X[, shapley_weight := r[["sample_frequence"]]]

  # Populate table and remove duplicated rows -------
  X[, features := feature_sample]
  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]

  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[between(n_features, 1, m - 1)]]
  X[ind, p := p[n_features]]
  X[ind, N := n[n_features]]

  # Set column order and key table
  data.table::setkeyv(X, "n_features")
  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
  data.table::setcolorder(X, nms)

  return(X)
}

#' @keywords internal
helper_feature <- function(m, feature_sample) {

  sample_frequence <- is_duplicate <- NULL # due to NSE notes in R CMD check

  x <- feature_matrix_cpp(feature_sample, m)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, sample_frequence := as.integer(.N), by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  dt[, (cnms) := NULL]

  return(dt)
}

#' Initiate the making of dummy variables
#'
#' @param traindata data.table or data.frame.
#'
#' @param testdata data.table or data.frame. New data that has the same
#' feature names, types, and levels as \code{traindata}.
#'
#' @return A list that contains the following entries:
#' \describe{
#' \item{obj}{List, Contains \describe{
#' \item{features}{Vector. Contains the names of all the features in \code{data}.}
#' \item{factor_features}{Vector. Contains the names of all the factors in \code{data}.}
#' \item{factor_list}{List. Contains each factor and its vector of levels.}
#' \item{contrasts_list}{List. Contains all the contrasts of the factors.}
#' }}
#' \item{train_dummies}{A data.frame containing all of the factors in \code{traindata} as
#' one-hot encoded variables.}
#' \item{test_dummies}{A data.frame containing all of the factors in \code{testdata} as
#' one-hot encoded variables.}
#' \item{traindata_new}{Original traindata with correct column ordering and factor levels.}
#' \item{testdata_new}{Original testdata with correct column ordering and factor levels. Important for
#' explain() function.}
#' }
#'
#' @export
#'
#' @author Annabelle Redelmeier
#'
#' @examples
#'
#' data("Boston", package = "MASS")
#' x_var <- c("lstat", "rm", "dis", "indus")
#' y_var <- "medv"
#' x_train <- as.data.frame(Boston[401:411, x_var])
#' y_train <- Boston[401:408, y_var]
#' x_test <- as.data.frame(Boston[1:4, x_var])
#'
#' # convert to factors for illustational purpose
#' x_train$rm <- factor(round(x_train$rm))
#' x_test$rm <- factor(round(x_test$rm), levels = levels(x_train$rm))
#'
#' dummylist <- make_dummies(traindata = x_train, testdata = x_test)
#'
make_dummies <- function(traindata, testdata) {

  contrasts <- features <- factor_features <- NULL # due to NSE notes in R CMD check
  # <- model.frame <- model.matrix
  if (is.null(colnames(traindata))) {
    stop("traindata must have column names.")
  }
  if (is.null(colnames(testdata))) {
    stop("testdata must have column names.")
  }

  traindata <- data.table::as.data.table(traindata)
  testdata <- data.table::as.data.table(testdata)

  if (length(colnames(traindata)) != length(colnames(testdata))) {
    stop("traindata and testdata must have the same number of columns.")
  }

  if (!all(sort(colnames(traindata)) == sort(colnames(testdata)))) {
    stop("traindata and testdata must have the same column names.")
  }

  features <- colnames(traindata)
  # feature names must be unique
  if (any(duplicated(features))) {
    stop("Both traindata and testdata must have unique column names.")
  }

  # Check if any features have empty names i.e ""
  if (any(features == "")) {
    stop("One or more features is missing a name.")
  }

  # In case the testing data has a different column order than the training data:
  testdata <- testdata[, features, with = FALSE]

  # Check if the features all have class "numeric" or "factor
  if (!all(sapply(traindata, class) %in% c("numeric", "factor"))) {
    stop("All traindata must have class numeric or factor.")
  }
  if (!all(sapply(testdata, class) %in% c("numeric", "factor"))) {
    stop("All testdata must have class numeric or factor.")
  }
  # Check if traindata and testdata have features with the same class
  if (!all(sapply(traindata, class) == sapply(testdata, class))) {
    stop("All traindata and testdata must have the same classes.")
  }

  # Check that traindata and testdata have the same levels for the factor features
  is_factor <- sapply(traindata, is.factor) # check which features are factors
  nb_factor <- sum(is_factor)

  list_levels_train <- lapply(traindata[, is_factor, with = FALSE], function(x) {sort(levels(x))})
  list_levels_test <- lapply(testdata[, is_factor, with = FALSE], function(x) {sort(levels(x))})

  if (!identical(list_levels_train, list_levels_test)) {
    stop("Levels of categorical variables in traindata and testdata must be the same.")
  }

  # re-level traindata and testdata
  for (i in names(list_levels_train)) {
    traindata[[i]] <- factor(traindata[[i]], levels = list_levels_train[[i]])
  }
  for (i in names(list_levels_test)) {
    testdata[[i]] <- factor(testdata[[i]], levels = list_levels_test[[i]])
  }

  if (nb_factor > 0) {
    factor_features <- features[is_factor]
    factor_list <- lapply(traindata[, factor_features, with = FALSE], levels)
  } else {
    factor_features <- NULL
    factor_list <- NULL
  }

  contrasts_list <- lapply(traindata[, factor_features, with = FALSE], contrasts, contrasts = FALSE)

  obj <- list(features = features,
              factor_features = factor_features,
              factor_list = factor_list,
              contrasts_list = contrasts_list,
              class_vector = sapply(traindata, class))

  # get train dummies
  m <- model.frame(data = traindata,
                   xlev = obj$factor_list)
  train_dummies <- model.matrix(object = ~. + 0,
                    data = m,
                    contrasts.arg = obj$contrasts_list)

  # get test dummies
  m <- model.frame(data = testdata,
                   xlev = obj$factor_list)
  test_dummies <- model.matrix(object = ~. + 0,
                               data = m,
                               contrasts.arg = obj$contrasts_list)


  return(list(obj = obj, train_dummies = train_dummies, test_dummies = test_dummies, traindata_new = traindata,
              testdata_new = testdata))

}

#' Make dummy variables - this is an internal function intended only to be used in
#' predict_model.xgb.Booster()
#'
#' @param obj List. Output of \code{make_dummies$obj}.
#'
#' @param testdata data.table or data.frame. New data that has the same
#' feature names, types, and levels as \code{obj$data}.
#'
#' @return A data.frame containing all of the factors in \code{testdata} as
#' one-hot encoded variables.
#'
#' @author Annabelle Redelmeier
#'
#' @keywords internal
#'
apply_dummies <- function(obj, testdata) {

  features <- NULL # due to NSE notes in R CMD check
  # model.frame <- model.matrix
  if (is.null(colnames(testdata))) {
    stop("testdata must have column names.")
  }

  testdata <- data.table::as.data.table(testdata)
  features <- obj$features

  if (length(features) != length(colnames(testdata))) {
    stop("traindata and testdata must have the same number of columns.")
  }
  if (!all(sort(features) == sort(colnames(testdata)))) {
    stop("traindata and testdata must have the same column names.")
  }
  if (any(duplicated(colnames(testdata)))) {
    stop("testdata must have unique column names.")
  }

  # in case the testing data has a different column order or more columns than the training data:
  testdata <- testdata[, features, with = FALSE]

  if (!all(sapply(testdata, class) %in% c("numeric", "factor"))) {
    stop("All testdata must have class numeric or factor.")
  }
  if (!all(obj$class_vector == sapply(testdata, class))) {
    stop("All traindata and testdata must have the same classes.")
  }

  # Check that traindata and testdata have the same levels for the factor features
  is_factor <- obj$factor_features
  list_levels_train <- obj$factor_list
  list_levels_test <- lapply(testdata[, is_factor, with = FALSE], function(x){sort(levels(x))})

  if (!identical(list_levels_train, list_levels_test)) {
    stop("Levels of categorical variables in traindata and testdata must be the same.")
  }

  # re-level testdata
  for (i in names(list_levels_test)) {
    testdata[[i]] <- factor(testdata[[i]], levels = list_levels_test[[i]])
  }

  m <- model.frame(data = testdata,
                   xlev = obj$factor_list)

  x <- model.matrix(object = ~. + 0,
                    data = m,
                    contrasts.arg = obj$contrasts_list)
  return(x)
}
