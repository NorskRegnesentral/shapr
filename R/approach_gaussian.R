#' @rdname setup_approach
#'
#' @param gaussian.mu Numeric vector.
#' Containing the mean of the data generating distribution.
#' `NULL` means it is estimated from the `x_train`.
#'
#' @param gaussian.cov_mat Numeric matrix.
#' Containing the covariance matrix of the data generating distribution.
#' `NULL` means it is estimated from the `x_train`.
#'
#' @inheritParams default_doc_export
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
    cli::cli_abort(paste0(
      "The following feature(s) are factor(s): ", paste0(factor_features, collapse = ", "), ". ",
      "approach = 'gaussian' does not support factor features. ",
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

#' @inheritParams default_doc_internal
#' @rdname prepare_data
#' @export
#' @author Martin Jullum,
#' @author Lars Henry Berge Olsen
prepare_data.gaussian <- function(internal, index_features, ...) {
  # Extract used variables
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_MC_samples <- internal$parameters$n_MC_samples
  n_coalitions_now <- length(index_features)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  causal_sampling <- internal$parameters$causal_sampling

  iter <- length(internal$iter_list)

  S <- internal$iter_list[[iter]]$S[index_features, , drop = FALSE]

  if (causal_sampling) {
    # Causal Shapley values (either symmetric or asymmetric)

    # Get if this is the first causal sampling step
    causal_first_step <- isTRUE(internal$parameters$causal_first_step) # Only set when called from prepare_data_causal

    # Set which gaussian data generating function to use
    prepare_gauss <- ifelse(causal_first_step, prepare_data_gaussian_cpp, prepare_data_gaussian_cpp_caus)

    # Set if we have to reshape the output of the prepare_gauss function
    reshape_prepare_gauss_output <- ifelse(causal_first_step, TRUE, FALSE)

    # For not the first step, the number of MC samples for causal Shapley values is n_explain; see prepare_data_causal
    n_MC_samples_updated <- ifelse(causal_first_step, n_MC_samples, n_explain)
  } else {
    # Regular Shapley values (either symmetric or asymmetric)

    # Set which gaussian data generating function to use
    prepare_gauss <- prepare_data_gaussian_cpp

    # Set if we have to reshape the output of the prepare_gauss function
    reshape_prepare_gauss_output <- TRUE

    # Set that the number of updated MC samples, only used when sampling from N(0, 1)
    n_MC_samples_updated <- n_MC_samples
  }

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_MC_samples_updated * n_features), nrow = n_MC_samples_updated, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}) for all coalitions and explicands.
  # The `dt` object is a 3D array of dimension (n_MC_samples, n_explain * n_coalitions, n_features) for regular
  # Shapley and in the first step for causal Shapley values. For later steps in the causal Shapley value framework,
  # the `dt` object is a matrix of dimension (n_explain * n_coalitions, n_features).
  dt <- prepare_gauss(MC_samples_mat = MC_samples_mat, x_explain_mat = x_explain_mat, S = S, mu = mu, cov_mat = cov_mat)

  # Reshape `dt` to a 2D array of dimension (n_MC_samples * n_explain * n_coalitions, n_features) when needed
  if (reshape_prepare_gauss_output) dim(dt) <- c(n_coalitions_now * n_explain * n_MC_samples, n_features)

  # Convert to a data.table and add extra identification columns
  dt <- data.table::as.data.table(dt)
  data.table::setnames(dt, feature_names)
  dt[, id_coalition := rep(seq_len(nrow(S)), each = n_MC_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_MC_samples, times = nrow(S))]
  dt[, w := 1 / n_MC_samples]
  dt[, id_coalition := index_features[id_coalition]]
  data.table::setcolorder(dt, c("id_coalition", "id", feature_names))

  return(dt)
}

#' get_cov_mat
#'
#' @inheritParams explain
#' @param min_eigen_value Numeric
#' Specifies the smallest allowed eigen value before the covariance matrix of `x_train` is assumed to not be
#' positive definite, and [Matrix::nearPD()] is used to find the nearest one.
#' @keywords internal
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
#' @keywords internal
get_mu_vec <- function(x_train) {
  unname(colMeans(x_train))
}

#' Generate marginal Gaussian data using Cholesky decomposition
#'
#' Given a multivariate Gaussian distribution, this function creates data from specified marginals of said distribution.
#'
#' @param n_MC_samples Integer. The number of samples to generate.
#' @param Sbar_features Vector of integers indicating which marginals to sample from.
#' @param mu Numeric vector containing the expected values for all features in the multivariate Gaussian distribution.
#' @param cov_mat Numeric matrix containing the covariance between all features
#' in the multivariate Gaussian distribution.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
create_marginal_data_gaussian <- function(n_MC_samples, Sbar_features, mu, cov_mat) {
  # Extract the sub covariance matrix for the selected features
  cov_submat <- cov_mat[Sbar_features, Sbar_features]

  # Perform the Cholesky decomposition of the covariance matrix
  chol_decomp <- chol(cov_submat)

  # Generate independent standard normal samples
  Z <- matrix(rnorm(n_MC_samples * length(Sbar_features)), nrow = n_MC_samples)

  # Transform the standard normal samples to have the desired covariance structure
  samples <- Z %*% chol_decomp

  # Shift by the mean vector
  samples <- sweep(samples, 2, mu[Sbar_features], "+")

  return(data.table(samples))
}
