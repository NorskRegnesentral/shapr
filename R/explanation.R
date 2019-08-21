#' Explaining the output of machine learning models with more accurately estimated Shapley values
#'
#' @param x 1
#' @param explainer 2
#' @param approach 3
#' @param prediction_zero 4
#' @param ... Soething
#'
#' @export
explain <- function(x, explainer, approach, prediction_zero, ...) {

  if (approach == "empirical") {
    class(x) <- "empirical"
  } else if (approach == "gaussian") {
    class(x) <- "gaussian"
  } else if (approach == "copula") {
    class(x) <- "copula"
  } else if (approach == "combined") {
    class(x) <- "copula"
  } else {
    str_error <- paste(
      "It seems that you passed a non-valid value for approach.",
      "It should be either 'empirical', 'gaussian', 'copula' or",
      "'combined'."
    )
    stop(str_error)
  }

  UseMethod("explain", x)
}

#' @rdname explain
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero, index_features, ...) {

  # Add arguments to explainer object
  explainer$x_test <- x
  explainer$approach <- approach
  fixed_sigma_vec = 0.1
  AICc_no_samp_per_optim = 1000
  AIC_optim_max_eval = 20
  AIC_optim_startval = 0.1
  w_threshold = 0.95

  # Get distance matrix ----------------
  browser()
  explainer$D <- distance_matrix(
    explainer$x_train,
    x,
    explainer$X$features
  )

  #

  # Generate data
  dt <- prepare_data(explainer, x_test)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)
}

#' @rdname explain
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
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)
}

#' @rdname explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, n_samples = 1e3) {

  # Setup
  explainer$n_samples <- n_samples
  explainer$x_test <- x
  explainer$approach <- approach

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

  explainer$mu <- rep(0, ncol(explainer$x_train))
  cov_mat <- stats::cov(x_train)
  eigen_values <- eigen(cov_mat)$values

  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }

  # Generate data
  dt <- prepare_data(explainer, x_test)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)

}

#' @rdname explain
#' @export
explain.combined <- function(x, explainer, approach, prediction_zero, ...) {

  # Setup
  explainer$n_samples <- n_samples
  explainer$x_test <- x
  explainer$approach <- approach

}
