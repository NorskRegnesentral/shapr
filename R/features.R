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
#' @param data data.table or data.frame. Includes all the features (both factors and possibly others).
#'
#' @param newdata data.table or data.frame. New data (features) that has the same
#' features as \code{data}.
#'
#' @return A list that contains the following entries:
#' \describe{
#' \item{obj}{List, Contains \describe{
#' \item{features}{Vector. Contains the names of all the features in \code{data}.}
#' \item{factor_features}{Vector. Contains the names of all the factors in \code{data}.}
#' \item{factor_list}{List. Contains each factor and its vector of levels.}
#' \item{contrasts_list}{List. Contains all the contrasts of the factors.}
#' }}
#' \item{model.matrix}{A data.frame containing all of the factors in \code{new_data} as
#' one-hot encoded variables.}
#' }
#'
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
#' dummylist <- make_dummies(data = rbind(x_train, x_test), newdata = x_test)
#'
make_dummies <- function(data, newdata) {

  contrasts <- features <- factor_features <- model.frame <- model.matrix <- NULL # due to NSE notes in R CMD check
  #
  if (is.null(colnames(data))) {
    stop("data must have column names.")
  }
  if (is.null(colnames(newdata))) {
    stop("newdata must have column names.")
  }
  if (is.null(newdata)) {
    stop("newdata needs to be included.")
  }

  data <- data.table::as.data.table(as.data.frame(data, stringsAsFactors = FALSE))
  newdata <- data.table::as.data.table(as.data.frame(newdata, stringsAsFactors = FALSE))

  # Check that data has unique names
  features <- colnames(data)
  if (length(unique(features)) < length(features)) {
    stop("Features must have unique names.")
  }

  # Check that all features in data are in newdata
  if (!all(features %in% names(newdata))) {
    stop("Some features missing from newdata.")
  }

  # Check that all features in data have the correct data type
  for (i in features) {
    if (class(newdata[[i]]) != class(data[[i]])) {
      stop("All features must have the same type as original data.")
    }
  }

  # Check that data and newdata have the same levels
  data0 <- data[, features, with = FALSE]
  is_factor_all <- sapply(data0, is.factor) # check which features are factors
  nb_factor_all <- sum(is_factor_all)
  list_levels_all <- lapply(data0[, is_factor_all, with = FALSE], levels)
  #
  newdata0 <- newdata[, features, with = FALSE]
  is_factor_new <- sapply(newdata0, is.factor) # check which features are factors
  nb_factor_new <- sum(is_factor_new)
  list_levels_new <- lapply(newdata0[, is_factor_new, with = FALSE], levels)

  for(i in names(list_levels_all)){
    if(!setequal(list_levels_new[[i]], list_levels_all[[i]])){
      stop("Levels of categorical variables in data and newdata must be the same!")
    }
  }

  if (nb_factor_all > 0) {
    factor_features <- features[is_factor_all]
    factor_list <- lapply(data[, factor_features, with = FALSE], levels)

  } else {
    factor_features <- NULL
    factor_list <- NULL
  }
  contrasts_list <- lapply(data[, factor_features, with = FALSE], contrasts, contrasts = FALSE)

  obj <- list(data = data,
              features = features,
              factor_features = factor_features,
              factor_list = factor_list,
              contrasts_list = contrasts_list)

  newdata_sub <- newdata[, features, with = FALSE]
  m <- model.frame(data = newdata_sub,
                   xlev = obj$factor_list)

  x <- model.matrix(object = ~. + 0,
                    data = m,
                    contrasts.arg = contrasts_list)

  return(list(obj = obj, model.matrix = x))

}

#' Make dummy variables - this is an internal function intended only to be used in
#' predict_model.xgb.Booster()
#'
#' @param obj List. Output of \code{make_dummies}.
#'
#' @param newdata data.table or data.frame. New data (features) that has the same
#' features as the data used in \code{make_dummies}.
#'
#' @return A data.frame containing all of the factors in \code{new_data} as
#' one-hot encoded variables.
#'
#' @author Annabelle Redelmeier
#'
#' @keywords internal
#'
apply_dummies <- function(obj, newdata) {

  features <- model.frame <- model.matrix <- NULL # due to NSE notes in R CMD check
  if (is.null(newdata)) {
    stop("newdata needs to be included.")
  }
  if (is.null(colnames(newdata))) {
    stop("newdata must have column names.")
  }
  newdata <- data.table::as.data.table(as.data.frame(newdata, stringsAsFactors = FALSE))


  # Check all features in data are also in newdata
  if (!all(obj$features %in% names(newdata))) {
    stop("Some features missing from newdata.")
  }

  # Check that all features in data have the correct data type
  for (i in obj$features) {
    if (class(newdata[[i]]) != class(obj$data[[i]])) {
      stop("All features must have the same type as original data.")
    }
  }

  # Check that data and newdata have the same levels
  data <- obj$data
  features <- obj$features
  data <- data[, features, with = FALSE]
  is_factor_all <- sapply(data, is.factor) # check which features are factors
  list_levels_all <- lapply(data[, is_factor_all, with = FALSE], levels)
  #
  newdata <- newdata[, features, with = FALSE]
  is_factor_new <- sapply(newdata, is.factor) # check which features are factors
  list_levels_new <- lapply(newdata[, is_factor_new, with = FALSE], levels)

  for(i in names(list_levels_all)){
    if(!setequal(list_levels_new[[i]], list_levels_all[[i]])){
      stop("Levels of categorical variables in data and newdata must be the same!")
    }
  }

  newdata_sub <- newdata[, features, with = FALSE]

  m <- model.frame(data = newdata_sub,
                   xlev = obj$factor_list)

  x <- model.matrix(object = ~. + 0,
                    data = m,
                    contrasts.arg = obj$contrasts_list)
  return(x)
}
