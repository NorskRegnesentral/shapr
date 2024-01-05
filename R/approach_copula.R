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
#' @author Lars Henry Berge Olsen
prepare_data.copula2 <- function(internal, index_features, ...) {

  # Extract used variables
  X <- internal$objects$X
  x_train <- internal$data$x_train
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain <- internal$data$x_explain
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_samples = 1000000
  n_features <- internal$parameters$n_features
  n_combinations <- internal$parameters$n_combinations
  n_combinations_now <- length(index_features)
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian) # CAN SKIP as.matrix as it is a matrix allready
  feature_names <- internal$parameters$feature_names









  S <- internal$objects$S[index_features, , drop = FALSE]


  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use Cpp to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}) for all coalitions and explicands.
  # The object `dt` is here a 3D array of dimension (n_samples, n_explain*n_coalitions, n_features).
  # INCLUDE THE TRANSFORMATION


  dt <- prepare_data_copula_cpp(MC_samples_mat = MC_samples_mat,
                                  x_explain_mat = x_explain_mat,
                                  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
                                  x_train_mat = x_train_mat,
                                  S = S,
                                  mu = copula.mu,
                                  cov_mat = copula.cov_mat)

  # Reshape `dt` to a 2D array of dimension (n_samples*n_explain*n_coalitions, n_features).
  dim(dt) = c(n_combinations_now*n_explain*n_samples, n_features)

  # Convert to a data.table
  dt = as.data.table(dt)
  setnames(dt, feature_names)
  dt[, "id_combination" := rep(seq(nrow(S)), each = n_samples * n_explain)]
  dt[, "id" := rep(seq(n_explain), each = n_samples, times = nrow(S))]
  dt[, "w" := 1 / n_samples]
  data.table::setcolorder(dt, c("id_combination", "id", feature_names))
  dt[, id_combination := index_features[id_combination]]
  dt

  dt[id_combination == 9 & id == 1,]

  dt_agr = dt[, lapply(.SD, mean), by = list(id, id_combination)]
  data.table::setorderv(dt_agr, c("id", "id_combination"))
  dt_agr

  return(dt)
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
prepare_data.copula <- function(internal, index_features = NULL, ...) {
  X <- internal$objects$X
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian <- internal$data$copula.x_explain_gaussian


  x_explain0 <- as.matrix(x_explain)
  dt_l <- list()
  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  n_samples = 1000000
  for (i in seq_len(n_explain)) {
    print(i)
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
    # x_explain_gaussian2 = x_explain_gaussian
    # x_train2 = x_train
    # x_explain2 = x_explain

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt
  dt9 = dt
  dt9_agr = dt9[, lapply(.SD, mean), by = list(id, id_combination)]
  dt9_agr - dt_agr
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

    # Dette har jeg kode til
    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_explain_gaussian[index_given]
    )

    # Dette har jeg kode til. Bruker cholensky + simulert data fra før
    ret0_z <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    # Dette må jeg skrive selv
    ret0_x <- apply(
      X = rbind(ret0_z, x_train[, dependent_ind, drop = FALSE]),
      MARGIN = 2,
      FUN = inv_gaussian_transform,
      n_z = n_samples,
      type = 5
    )
    ret0_x_cpp = inv_gaussian_transform_cpp(z = ret0_z, x = x_train[, dependent_ind, drop = FALSE])

    ret0_x_cpp = inv_gaussian_transform_cpp_armamat(rbind(ret0_z, x_train[, dependent_ind, drop = FALSE]), n_samples = n_samples)
    colnames(ret0_x_cpp) = feature_names[dependent_ind]
    all.equal(ret0_x, ret0_x_cpp)

    # Dette har jeg kode til
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
inv_gaussian_transform <- function(zx, n_z, type) {
  if (n_z >= length(zx)) stop("n_z should be less than length(zx)")
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  x_new <- stats::quantile(x, probs = u, type = type)
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
