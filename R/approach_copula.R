#' @rdname setup_approach
#' @inheritParams default_doc_export
#' @export
#' @author Martin Jullum
setup_approach.copula <- function(internal, ...) {
  parameters <- internal$parameters
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)

  # Checking if factor features are present
  feature_specs <- internal$objects$feature_specs
  if (any(feature_specs$classes == "factor")) {
    factor_features <- names(which(feature_specs$classes == "factor"))
    factor_approaches <- get_factor_approaches()
    cli::cli_abort(
      paste0(
        "The following feature(s) are factor(s): ", paste0(factor_features, collapse = ", "), ". ",
        "approach = 'copula' does not support factor features.",
        "Please change approach to one of ", paste0(factor_approaches, collapse = ", "), "."
      )
    )
  }

  # Prepare transformed data
  parameters$copula.mu <- rep(0, ncol(x_train_mat))
  x_train_mat0 <- apply(X = x_train_mat, MARGIN = 2, FUN = gaussian_transform)
  parameters$copula.cov_mat <- get_cov_mat(x_train_mat0)

  x_explain_gaussian <- apply(
    X = rbind(x_explain_mat, x_train_mat),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(x_explain_mat)
  )
  if (is.null(dim(x_explain_gaussian))) x_explain_gaussian <- t(as.matrix(x_explain_gaussian))

  # Add objects to internal list
  internal$parameters <- parameters
  internal$data$copula.x_explain_gaussian <- as.data.table(x_explain_gaussian)

  return(internal)
}

#' @inheritParams default_doc_internal
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.copula <- function(internal, index_features, ...) {
  # Extract used variables
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_MC_samples <- internal$parameters$n_MC_samples
  n_features <- internal$parameters$n_features
  n_coalitions_now <- length(index_features)
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian)
  causal_sampling <- internal$parameters$causal_sampling

  iter <- length(internal$iter_list)

  S <- internal$iter_list[[iter]]$S[index_features, , drop = FALSE]

  if (causal_sampling) {
    # Causal Shapley values (either symmetric or asymmetric)

    # Get if this is the first causal sampling step
    causal_first_step <- isTRUE(internal$parameters$causal_first_step) # Only set when called from prepare_data_causal

    # Set which copula data generating function to use
    prepare_copula <- ifelse(causal_first_step, prepare_data_copula_cpp, prepare_data_copula_cpp_caus)

    # Set if we have to reshape the output of the prepare_gauss function
    reshape_prepare_copula_output <- ifelse(causal_first_step, TRUE, FALSE)

    # For not the first step, the number of MC samples for causal Shapley values is n_explain; see prepare_data_causal
    n_MC_samples_updated <- ifelse(causal_first_step, n_MC_samples, n_explain)


    # Update data when not in the first causal sampling step; see prepare_data_causal for explanations
    if (!causal_first_step) {
      # Update the `copula.x_explain_gaussian_mat`
      copula.x_explain_gaussian <- apply(
        X = rbind(x_explain_mat, x_train_mat),
        MARGIN = 2,
        FUN = gaussian_transform_separate,
        n_y = nrow(x_explain_mat)
      )
      if (is.null(dim(copula.x_explain_gaussian))) copula.x_explain_gaussian <- t(as.matrix(copula.x_explain_gaussian))
      copula.x_explain_gaussian_mat <- as.matrix(copula.x_explain_gaussian)
    }
  } else {
    # Regular Shapley values (either symmetric or asymmetric)

    # Set if we have to reshape the output of the prepare_copula function
    reshape_prepare_copula_output <- TRUE

    # Set which copula data generating function to use
    prepare_copula <- prepare_data_copula_cpp

    # Set that the number of updated MC samples, only used when sampling from N(0, 1)
    n_MC_samples_updated <- n_MC_samples
  }

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_MC_samples_updated * n_features), nrow = n_MC_samples_updated, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
  # and then transforming them back to the original scale using the inverse Gaussian transform in C++.
  # The `dt` object is a 3D array of dimension (n_MC_samples, n_explain * n_coalitions, n_features) for regular
  # Shapley and in the first step for causal Shapley values. For later steps in the causal Shapley value framework,
  # the `dt` object is a matrix of dimension (n_explain * n_coalitions, n_features).
  dt <- prepare_copula(
    MC_samples_mat = MC_samples_mat,
    x_explain_mat = x_explain_mat,
    x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
    x_train_mat = x_train_mat,
    S = S,
    mu = copula.mu,
    cov_mat = copula.cov_mat
  )

  # Reshape `dt` to a 2D array of dimension (n_MC_samples * n_explain * n_coalitions, n_features) when needed
  if (reshape_prepare_copula_output) dim(dt) <- c(n_coalitions_now * n_explain * n_MC_samples, n_features)

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
  if (n_y >= length(yx)) cli::cli_abort("n_y should be less than length(yx)")
  ind <- 1:n_y
  x <- yx[-ind]
  tmp <- rank(yx)[ind]
  tmp <- tmp - rank(tmp) + 0.5
  u_y <- tmp / (length(x) + 1)
  z_y <- stats::qnorm(u_y)
  return(z_y)
}
