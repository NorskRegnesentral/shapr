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
#' \item{ID}{Positive integer. Represents a unique key for each combination. Note that the table
#' is sorted by \code{ID}, so that is always equal to \code{x[["ID"]] = 1:nrow(x)}.}
#' \item{features}{List. Each item of the list is an integer vector where \code{features[[i]]}
#' represents the indices of the features included in combination \code{i}. Note that all the items
#' are sorted such that \code{features[[i]] == sort(features[[i]])} is always true.}
#' \item{nfeatures}{Positive integer. \code{nfeatures[i]} equals the number of features in combination
#' \code{i}, i.e. \code{nfeatures[i] = length(features[[i]])}.}.
#' \item{N}{Positive integer. The number of unique ways to sample \code{nfeatures[i]} features
#' from \code{m} different features, without replacement.}
#' }
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite, Martin Jullum
#'
#' @examples
#' # All combinations
#' x <- shapr:::feature_combinations(m = 5)
#' nrow(x) # Equals 2^5 = 32
#'
#' # Subsample of combinations
#' x <- shapr:::feature_combinations(m = 13, n_combinations = 1e3)
feature_combinations <- function(m, exact = TRUE, n_combinations = 200, weight_zero_m = 10^6) {

  # Force user to use a natural number for n_combinations if m > 12
  if (m > 12 & is.null(n_combinations)) {
    stop(
      paste0(
        "Due to computational complexity, we recommend setting n_combinations = 10 000\n",
        "if the number of features is larger than 12. Note that you can force the use of the exact\n",
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
    cat(sprintf("n_combinations is larger than or equal to 2^m = %d. Using exact instead.", 2^m))
  }

  if (exact) {
    dt <- feature_exact(m, weight_zero_m)
  } else {
    dt <- feature_not_exact(m, n_combinations, weight_zero_m)
  }

  return(dt)
}

#' @keywords internal
feature_exact <- function(m, weight_zero_m = 10^6) {

  dt <- data.table::data.table(ID = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
  dt[, features := unlist(combinations, recursive = FALSE)]
  dt[, nfeatures := length(features[[1]]), ID]
  dt[, N := .N, nfeatures]
  dt[, shapley_weight := shapley_weights(m = m, N = N, s = nfeatures, weight_zero_m)]
  dt[, no := 1]

  return(dt)
}

#' @keywords internal
feature_not_exact <- function(m, n_combinations = 200, weight_zero_m = 10^6, reduce_dim = TRUE) {

  # Find weights for given number of features ----------
  nfeatures <- seq(m - 1)
  n <- sapply(nfeatures, choose, n = m)
  w <- shapley_weights(m = m, N = n, s = nfeatures) * n
  p <- w / sum(w)

  # Sample number of chosen features ----------
  X <- data.table::data.table(
    nfeatures = c(
      0,
      sample(
        x = nfeatures,
        size = n_combinations,
        replace = TRUE,
        prob = p
      ),
      m
    )
  )
  X[, nfeatures := as.integer(nfeatures)]

  # Sample specific set of features -------
  data.table::setkey(X, nfeatures)
  feature_sample <- sample_features_cpp(m, X[["nfeatures"]])

  # Get number of occurences and duplicated rows-------
  r <- helper_feature(m, feature_sample)
  X[, no := r[["no"]]]
  X[, is_duplicate := r[["is_duplicate"]]]
  X[, ID := .I]

  # Populate table and remove duplicated rows -------
  X[, features := feature_sample]
  if (reduce_dim && any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
    X[, no := 1]
  }
  X[, is_duplicate := NULL]
  nms <- c("ID", "nfeatures", "features", "no")
  data.table::setcolorder(X, nms)

  # Add shapley weight and number of combinations
  X[, shapley_weight := weight_zero_m]
  X[, N := 1]
  X[between(nfeatures, 1, m - 1), ind := TRUE]
  X[ind == TRUE, shapley_weight := p[nfeatures]]
  X[ind == TRUE, N := n[nfeatures]]
  X[, ind := NULL]

  # Set column order and key table
  nms <- c("ID", "features", "nfeatures", "N", "shapley_weight", "no")
  data.table::setcolorder(X, nms)
  data.table::setkey(X, nfeatures)
  X[, ID := .I]
  X[, N := as.integer(N)]

  return(X)
}

#' @keywords internal
helper_feature <- function(m, feature_sample) {

  x <- feature_matrix_cpp(feature_sample, m)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, no := as.integer(.N), by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  dt[, (cnms) := NULL]

  return(dt)
}
