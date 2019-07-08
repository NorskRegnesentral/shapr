#' Get combinations
#'
#' @inheritParams global_arguments
#'
#' @details
#' The returned data.table contains the following columns
#' \describe{
#' \item{ID}{Positive integer. Unique key for combination}
#' \item{features}{List}
#' \item{nfeautres}{Positive integer}
#' \item{N}{Positive integer}
#' }
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
feature_combinations <- function(m, exact = TRUE, noSamp = 200, weight_zero_m = 10^6, reduce_dim = T) {

  if (!exact && noSamp > (2^m - 2) && !replace) {
    noSamp <- 2^m - 2
    cat(sprintf("noSamp is larger than 2^m = %d. Using exact instead.", 2^m))
  }

  dt <- ifelse(
    exact,
    feature_exact(m, weight_zero_m),
    feature_not_exact(m, noSamp, weight_zero_m, reduce_dim),
  )

  return(dt)
}

#' @keywords internal
#' @export
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
#' @export
feature_not_exact <- function(m, noSamp = 200, weight_zero_m = 10^6, reduce_dim = T) {

  m <- 20
  noSamp = 1e4
  weight_zero_m = 10^6
  reduce_dim = T

  # Find weights for given number of features ----------
  nfeatures <- seq(m - 1)
  n <- sapply(nfeatures, choose, n = m)
  w <- shapley_weights(m = m, N = n, s = nfeatures) * n
  p <- w / sum(w)

  # Sample number of features ----------
  X <- data.table::data.table(
    nfeatures = c(
      0,
      sample(
        x = nfeatures,
        size = noSamp,
        replace = TRUE,
        prob = p
      ),
      m
    )
  )

  # Sample specific set of features -------
  data.table::setkey(X, nfeatures)
  feature_sample <- sample_features_cpp(m, X$nfeatures)

  # Get number of occurences and remove duplicated rows -------
  r <- helper_feature(m, feature_sample)
  X[, no := r[["no"]]]
  X[, is_duplicate := r[["is_duplicate"]]]
  X[, nfeatures := r[["nfeatures"]]]
  X[, ID := .I]

  # Populate data.table -------
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

  return(X)
}

#' @keywords internal
helper_feature <- function(m, feature_sample) {

  x <- helper_feature_matrix(m, feature_sample)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, nfeatures := rowSums(x)]
  dt[, no := .N, by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  cnms <- c("nfeatures", "no", "is_duplicate")

  return(dt[, .SD, .SDcols = cnms])
}
