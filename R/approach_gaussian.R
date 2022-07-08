#' @keywords internal
setup_approach.gaussian <- function(internal,
                                    mu = NULL,
                                    cov_mat = NULL, ...) {

  parameters <- internal$parameters
  x_train <- internal$data$x_train

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    parameters$mu <- get_mu_vec(x_train)
  } else {
    parameters$mu <- mu
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    parameters$cov_mat <- get_cov_mat(x_train)
  } else {
    parameters$cov_mat <- cov_mat
  }

  internal$parameters <- parameters

  return(internal)
}

#' @rdname prepare_data
#' @export
prepare_data.gaussian <- function(internal, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  cov_mat <- internal$parameters$cov_mat
  n_samples <- internal$parameters$n_samples
  mu <- internal$parameters$mu
  n_features <- internal$parameters$n_features

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
      FUN = sample_gaussian,
      n_samples = n_samples,
      mu = mu,
      cov_mat = cov_mat,
      m = n_features,
      x_explain = x_explain0[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}

#' get_cov_mat
#'
#' @inheritParams explain
#' @param min_eigen_value Numeric
#' Specifies the smallest allowed eigen value before the covariance matrix of \code{x_train} is assumed to not be
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

#' Sample conditional Gaussian variables
#'
#' @inheritParams sample_copula
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Martin Jullum
sample_gaussian <- function(index_given, n_samples, mu, cov_mat, m, x_explain) {

  # Check input
  stopifnot(is.matrix(x_explain))

  # Handles the unconditional and full conditional separtely when predicting
  cnms <- colnames(x_explain)
  if (length(index_given) %in% c(0, m)) {
    return(data.table::as.data.table(x_explain))
  }

  dependent_ind <- (1:length(mu))[-index_given]
  x_explain_gaussian <- x_explain[index_given]
  tmp <- condMVNorm::condMVN(
    mean = mu,
    sigma = cov_mat,
    dependent.ind = dependent_ind,
    given.ind = index_given,
    X.given = x_explain_gaussian
  )

  # Makes the conditional covariance matrix symmetric in the rare case where numerical instability made it unsymmetric
  if (!isSymmetric(tmp[["condVar"]])) {
    tmp[["condVar"]] <- Matrix::symmpart(tmp$condVar)
  }

  ret0 <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

  ret <- matrix(NA, ncol = m, nrow = n_samples)
  ret[, index_given] <- rep(x_explain_gaussian, each = n_samples)
  ret[, dependent_ind] <- ret0

  colnames(ret) <- cnms
  return(as.data.table(ret))
}
