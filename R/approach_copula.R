#' @rdname setup_approach
#' @inheritParams default_doc_explain
#' @export
#' @author Martin Jullum
setup_approach.copula <- function(internal, ...) {
  parameters <- internal$parameters
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  # Checking if factor features are present
  feature_specs <- internal$objects$feature_specs
  if (any(feature_specs$classes == "factor")) {
    factor_features <- names(which(feature_specs$classes == "factor"))
    factor_approaches <- get_factor_approaches()
    stop(
      paste0(
        "The following feature(s) are factor(s): ", factor_features, ".\n",
        "approach = 'copula' does not support factor features.\n",
        "Please change approach to one of ", paste0(factor_approaches, collapse = ", "), "."
      )
    )
  }

  # Prepare transformed data
  parameters$copula.mu <- rep(0, ncol(x_train))
  x_train0 <- apply(
    X = x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  parameters$copula.cov_mat <- get_cov_mat(x_train0)

  x_explain_gaussian <- apply(
    X = rbind(x_explain, x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(x_explain)
  )

  if (is.null(dim(x_explain_gaussian))) {
    x_explain_gaussian <- t(as.matrix(x_explain_gaussian))
  }
  # TODO: Change to this a data.table for consistency (not speed/memory)
  internal$data$copula.x_explain_gaussian <- x_explain_gaussian
  internal$parameters <- parameters

  return(internal)
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.copula <- function(internal, index_features, ...) {
  # Extract used variables
  S <- internal$objects$S[index_features, , drop = FALSE]
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  n_combinations_now <- length(index_features)
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian)

  # TODO: Note that `as.matrix` is not needed for `copula.x_explain_gaussian_mat` as it is already defined as a matrix
  # in `setup_approach.copula`, however, it seems that Martin plans to make it into a data.table, thus, I include
  # `as.matrix` as future safety. DISCUSS WITH MARTIN WHAT HIS PLANS ARE!

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
  # and then transforming them back to the original scale using the inverse Gaussian transform in C++.
  # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
  dt <- prepare_data_copula_cpp(
    MC_samples_mat = MC_samples_mat,
    x_explain_mat = x_explain_mat,
    x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
    x_train_mat = x_train_mat,
    S = S,
    mu = copula.mu,
    cov_mat = copula.cov_mat
  )

  # Reshape `dt` to a 2D array of dimension (n_samples * n_explain * n_coalitions, n_features).
  dim(dt) <- c(n_combinations_now * n_explain * n_samples, n_features)

  # Convert to a data.table and add extra identification columns
  dt <- data.table::as.data.table(dt)
  data.table::setnames(dt, feature_names)
  dt[, id_combination := rep(seq_len(nrow(S)), each = n_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_samples, times = nrow(S))]
  dt[, w := 1 / n_samples]
  dt[, id_combination := index_features[id_combination]]
  data.table::setcolorder(dt, c("id_combination", "id", feature_names))

  return(dt)
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.copula_cpp_and_R <- function(internal, index_features, ...) {
  # Extract used variables
  S <- internal$objects$S[index_features, , drop = FALSE]
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  n_combinations_now <- length(index_features)
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian)

  # TODO: Note that `as.matrix` is not needed for `copula.x_explain_gaussian_mat` as it is already defined as a matrix
  # in `setup_approach.copula`, however, it seems that Martin plans to make it into a data.table, thus, I include
  # `as.matrix` as future safety. DISCUSS WITH MARTIN WHAT HIS PLANS ARE!

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
  # and then transforming them back to the original scale using the inverse Gaussian transform in C++.
  # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
  dt <- prepare_data_copula_cpp_and_R(
    MC_samples_mat = MC_samples_mat,
    x_explain_mat = x_explain_mat,
    x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
    x_train_mat = x_train_mat,
    S = S,
    mu = copula.mu,
    cov_mat = copula.cov_mat
  )

  # Reshape `dt` to a 2D array of dimension (n_samples * n_explain * n_coalitions, n_features).
  dim(dt) <- c(n_combinations_now * n_explain * n_samples, n_features)

  # Convert to a data.table and add extra identification columns
  dt <- data.table::as.data.table(dt)
  data.table::setnames(dt, feature_names)
  dt[, id_combination := rep(seq_len(nrow(S)), each = n_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_samples, times = nrow(S))]
  dt[, w := 1 / n_samples]
  dt[, id_combination := index_features[id_combination]]
  data.table::setcolorder(dt, c("id_combination", "id", feature_names))

  return(dt)
}

#' Transform data using the inverse Gaussian transformation.
#'
#' @details This function is called from `prepare_data_copula_cpp()` as the this was faster
#'
#' @param z Matrix. The data are the Gaussian Monte Carlos samples to transform.
#' @param x Matrix. The data with the original transformation. Used to conduct the transformation of `z`.
#'
#' @return Matrix of the same dimension as `z`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
inv_gaussian_transform_R <- function(z, x) {
  u <- stats::pnorm(z)
  x_new <- sapply(seq_len(ncol(u)), function(idx) quantile.type7(x[, idx], probs = u[, idx]))
  return(x_new)
}

#' Compute the quantiles using quantile type seven
#'
#' @details Using quantile type number 7 from stats::quantile.
#'
#' @param x numeric vector whose sample quantiles are wanted.
#' @param probs numeric vector of probabilities with values between zero and one.
#'
#' @return A vector of length length(`probs`) is returned.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
quantile.type7 <- function(x, probs) {
  n <- length(x)
  probs <- pmax(0, pmin(1, probs)) # allow for slight overshoot
  index <- 1 + (n - 1) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  x <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]
  i <- which(index > lo)
  h <- (index - lo)[i]
  qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
  return(qs)
}

#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' @param yx Numeric vector. The first `n_y` items is the data that is transformed, and last
#' part is the data with the original transformation.
#' @param n_y Positive integer. Number of elements of `yx` that belongs to the Gaussian data.
#'
#' @return Vector of back-transformed Gaussian data
#'
#' @keywords internal
#' @author Martin Jullum
gaussian_transform_separate <- function(yx, n_y) {
  if (n_y >= length(yx)) stop("n_y should be less than length(yx)")
  ind <- 1:n_y
  x <- yx[-ind]
  tmp <- rank(yx)[ind]
  tmp <- tmp - rank(tmp) + 0.5
  u_y <- tmp / (length(x) + 1)
  z_y <- stats::qnorm(u_y)
  return(z_y)
}

#' Transforms a sample to standardized normal distribution
#'
#' @param x Numeric vector.The data which should be transformed to a standard normal distribution.
#'
#' @return Numeric vector of length `length(x)`
#'
#' @keywords internal
#' @author Martin Jullum
gaussian_transform <- function(x) {
  u <- rank(x) / (length(x) + 1)
  z <- stats::qnorm(u)
  return(z)
}
