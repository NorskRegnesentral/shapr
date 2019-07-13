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

#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero, index_features) {

  # Get distance matrix ----------------
  explainer$D <- distance_matrix(
    explainer$Xtrain,
    x,
    explainer$X
  )

  # Predict

  # Process

  # Return

  return()
}

#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL) {

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {

    mu <- unname(colMeans(explainer$x_train))
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    cov_mat <- stats::cov(explainer$x_train)
  }

  # Make sure that covariance matrix is positive-definite
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  }

  # Predict

  # Process

  # Return

  return(mu)

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

  # Predict

  # Process

  # Return

}
