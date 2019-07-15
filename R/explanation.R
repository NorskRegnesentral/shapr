#' hey
#' @export
explain <- function(x, explainer, approach, prediction_zero, ...) {

  if (approach == "empirical") {
    class(x) <- "empirical"
  } else if (approach == "gaussian") {
    class(x) <- "gaussian"
  } else if (approach == "copula") {
    class(x) <- "copula"
  } else {
    stop("It seems that you passed a non-valid value for approach. It should be either 'empirical', 'gaussian' or 'copula'.")
  }

  UseMethod("explain", x)
}

#' hey
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero, index_features) {

  # Get distance matrix ----------------
  explainer$D <- distance_matrix(
    explainer$x_train,
    x,
    explainer$X
  )

  # Predict

  # Process

  # Return

  return()
}

#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, n_samples = 1e3) {

  # Add arguments to explainer object
  explainer$n_samples <- n_samples
  explainer$x_test <- x
  explainer$approach <- approach

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    explainer$mu <- unname(colMeans(explainer$x_train))
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    cov_mat <- stats::cov(explainer$x_train)
  }

  # Make sure that covariance matrix is positive-definite
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }

  # Generate data
  dt <- prepare_data(explainer)

  # Predict
  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "wcomb"))
  dt[, p_hat := predict_model(x = explainer$model, newdata = .SD), .SDcols = cnms]
  dt[wcomb == 1, p_hat := prediction_zero]

  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, wcomb)]
  data.table::setkeyv(dt_res, c("id", "wcomb"))

  # Get mean probability
  kshap <- matrix(0.0, nrow(explainer$W), nrow(explainer$x_test))
  for (j in 1:ncol(kshap)) {

    kshap[, j] <- explainer$W %*% dt_res[id == j, k]
  }
  dt_kshap <- as.data.table(t(kshap))
  colnames(dt_kshap) <- c("none", colnames(explainer$x_train))

  return(dt_kshap)
}

#' @export
explain.copula <- function(x, ...) {

  # Prepare data
  x_train <- apply(
    X = explainer$x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  x_test <- apply(
    X = rbind(explainer$x_test, explainer$x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(explainer$x_test)
  )

  mu <- rep(0, ncol(explainer$x_train))
  cov_mat <- stats::cov(x_train)
  eigen_values <- eigen(x_train)$values
  if (any(eigen_values <= 1e-06)) {
    cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  }

  # Generate data

  # Predict

  # Process

  # Return

}

#' @export
prepare_data <- function(x){

  class(x) <- x$approach
  UseMethod("prepare_data", x)
}

#' @export
prepare_data.empirical <- function(x){
  UseMethod("prepare_data", x)
}

#' @export
prepare_data.gaussian <- function(x){

  n_xtest <- nrow(x$x_test)
  dt_l <- list()
  for (i in seq(n_xtest)) {
    l <- lapply(
      X = x$X$features,
      FUN = sample_gaussian,
      noSamp_MC = x$n_samples,
      mu = x$mu,
      Sigma = x$cov_mat,
      p = ncol(x$x_test),
      Xtest = x$x_test,
      ensure_condcov_symmetry = TRUE
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "wcomb")
    # dt[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    dt_l[[i]][, w := 1 / x$n_samples]
    dt_l[[i]][, id := i]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

  return(dt)
}

#' @export
prepare_data.copula <- function(x){

  samp_list <- lapply(
    X = x$X$features,
    FUN = sample_copula,
    noSamp_MC = x$n_samples,
    mu = x$mu,
    Sigma = x$mu,
    p = ncol(x$x_test),
    Xtest_Gauss_trans = x$x_test,
    Xtrain = Xtrain,
    Xtest = Xtest
  )

  DTp.copula <- rbindlist(samp_list, idcol = "wcomb")
  DTp.copula[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
  DTp.copula[, w := 1 / noSamp_MC]
}
