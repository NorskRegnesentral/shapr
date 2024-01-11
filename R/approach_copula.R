#' @rdname setup_approach
#' @inheritParams default_doc_explain
#' @export
#' @author Martin Jullum
setup_approach.copula <- function(internal, ...) {
  parameters <- internal$parameters
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  feature_names <- internal$parameters$feature_names

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
  parameters$copula.mu <- rep(0, ncol(x_train_mat))
  x_train_mat0 <- gaussian_transform_cpp(x_train_mat)
  colnames(x_train_mat0) <- feature_names
  parameters$copula.cov_mat <- get_cov_mat(x_train_mat0)

  x_explain_gaussian <- gaussian_transform_separate_cpp(x_explain_mat, x_train_mat)
  colnames(x_explain_gaussian) <- feature_names
  if (is.null(dim(x_explain_gaussian))) x_explain_gaussian <- t(as.matrix(x_explain_gaussian))

  # Add objects to internal list
  internal$parameters <- parameters
  internal$data$copula.x_explain_gaussian <- as.data.table(x_explain_gaussian)

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
