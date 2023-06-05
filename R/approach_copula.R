#' @rdname setup_approach
#'
#' @inheritParams default_doc_explain
#'
#' @export
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
prepare_data.copula <- function(internal, index_features = NULL, ...) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  copula.cov_mat <- internal$parameters$copula.cov_mat
  n_samples <- internal$parameters$n_samples
  copula.mu <- internal$parameters$copula.mu
  n_features <- internal$parameters$n_features

  copula.x_explain_gaussian <- internal$data$copula.x_explain_gaussian
  X <- internal$objects$X


  x_explain0 <- as.matrix(x_explain)
  dt_l <- list()
  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  for (i in seq_len(n_explain)) {
    l <- lapply(
      X = features,
      FUN = sample_copula,
      n_samples = n_samples,
      mu = copula.mu,
      cov_mat = copula.cov_mat,
      m = n_features,
      x_explain = x_explain0[i, , drop = FALSE],
      x_train = as.matrix(x_train),
      x_explain_gaussian = copula.x_explain_gaussian[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}

#' Sample conditional variables using the Gaussian copula approach
#'
#' @param index_given Integer vector. The indices of the features to condition upon. Note that
#' `min(index_given) >= 1` and `max(index_given) <= m`.
#' @param m Positive integer. The total number of features.
#' @param x_explain_gaussian Numeric matrix. Contains the observation whose predictions ought
#' to be explained (test data),
#' after quantile-transforming them to standard Gaussian variables.
#' @param x_explain Numeric matrix. Contains the features of the observation whose
#' predictions ought to be explained (test data).
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Martin Jullum
sample_copula <- function(index_given, n_samples, mu, cov_mat, m, x_explain_gaussian, x_train, x_explain) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(index_given) %in% c(0, m)) {
    ret <- matrix(x_explain, ncol = m, nrow = 1)
  } else {
    dependent_ind <- (seq_len(length(mu)))[-index_given]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_explain_gaussian[index_given]
    )

    ret0_z <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    ret0_x <- apply(
      X = rbind(ret0_z, x_train[, dependent_ind, drop = FALSE]),
      MARGIN = 2,
      FUN = inv_gaussian_transform,
      n_z = n_samples
    )

    ret <- matrix(NA, ncol = m, nrow = n_samples)
    ret[, index_given] <- rep(x_explain[index_given], each = n_samples)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(x_explain)
  return(as.data.table(ret))
}


#' Transforms new data to a standardized normal distribution
#'
#' @param zx Numeric vector. The first `n_z` items are the Gaussian data, and the last part is
#' the data with the original transformation.
#' @param n_z Positive integer. Number of elements of `zx` that belongs to new data.
#'
#' @return Numeric vector of length `n_z`
#'
#' @keywords internal
#'
#' @author Martin Jullum
inv_gaussian_transform <- function(zx, n_z) {
  if (n_z >= length(zx)) stop("n_z should be less than length(zx)")
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  x_new <- stats::quantile(x, probs = u)
  return(as.double(x_new))
}

#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' @param yx Numeric vector. The first `n_y` items is the data that is transformed, and last
#' part is the data with the original transformation.
#' @param n_y Positive integer. Number of elements of `yx` that belongs to the gaussian data.
#'
#' @return Vector of back-transformed Gaussian data
#'
#' @keywords internal
#'
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
#'
#' @author Martin Jullum
gaussian_transform <- function(x) {
  u <- rank(x) / (length(x) + 1)
  z <- stats::qnorm(u)
  return(z)
}
