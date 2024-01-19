#' @rdname setup_approach
#'
#' @param gaussian.mu Numeric vector. (Optional)
#' Containing the mean of the data generating distribution.
#' `NULL` means it is estimated from the `x_train`.
#'
#' @param gaussian.cov_mat Numeric matrix. (Optional)
#' Containing the covariance matrix of the data generating distribution.
#' `NULL` means it is estimated from the `x_train`.
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach.gaussian <- function(internal,
                                    gaussian.mu = NULL,
                                    gaussian.cov_mat = NULL, ...) {
  # For consistency
  defaults <- mget(c("gaussian.mu", "gaussian.cov_mat"))
  internal <- insert_defaults(internal, defaults)

  x_train <- internal$data$x_train
  feature_specs <- internal$objects$feature_specs

  # Checking if factor features are present
  if (any(feature_specs$classes == "factor")) {
    factor_features <- names(which(feature_specs$classes == "factor"))
    factor_approaches <- get_factor_approaches()
    stop(paste0(
      "The following feature(s) are factor(s): ", factor_features, ".\n",
      "approach = 'gaussian' does not support factor features.\n",
      "Please change approach to one of ", paste0(factor_approaches, collapse = ", "), "."
    ))
  }

  # If gaussian.mu is not provided directly in internal list, use mean of training data
  if (is.null(internal$parameters$gaussian.mu)) {
    internal$parameters$gaussian.mu <- get_mu_vec(x_train)
  }

  # If gaussian.cov_mat is not provided directly in internal list, use sample covariance of training data
  if (is.null(internal$parameters$gaussian.cov_mat)) {
    internal$parameters$gaussian.cov_mat <- get_cov_mat(x_train)
  }

  return(internal)
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.gaussian <- function(internal, index_features, ...) {
  # Extract used variables
  S <- internal$objects$S[index_features, , drop = FALSE]
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  n_combinations_now <- length(index_features)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use Cpp to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}) for all coalitions and explicands.
  # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
  dt <- prepare_data_gaussian_cpp(
    MC_samples_mat = MC_samples_mat,
    x_explain_mat = x_explain_mat,
    S = S,
    mu = mu,
    cov_mat = cov_mat
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

#' get_cov_mat
#'
#' @inheritParams explain
#' @param min_eigen_value Numeric
#' Specifies the smallest allowed eigen value before the covariance matrix of `x_train` is assumed to not be
#' positive definite, and [Matrix::nearPD()] is used to find the nearest one.
#' @export
get_cov_mat <- function(x_train, min_eigen_value = 1e-06) {
  cov_mat <- stats::cov(x_train)
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= min_eigen_value)) {
    cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  }
  return(cov_mat)
}

#' get_mu_vec
#'
#' @inheritParams explain
#' @export
get_mu_vec <- function(x_train) {
  unname(colMeans(x_train))
}
