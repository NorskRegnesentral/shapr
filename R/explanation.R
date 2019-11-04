#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#' @description TODO: Add a more detailed description
#'
#' @param x A matrix or data.frame. Contains the the features, whose
#' predictions ought to be explained (test data).
#'
#' @param explainer An \code{explainer} object to use for explaining the observations.
#' See \code{\link{shapr}}.
#'
#' @param approach Character vector of length \code{1} or \code{n_features}.
#' \code{n_features} equals the total number of features in the model. All elements should
#' either be \code{"gaussian"}, \code{"copula"} or \code{"empirical"}. See details for more information.
#'
#' @param prediction_zero The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param ... Additional arguments passed to \code{\link{prepare_data}}
#'
#' @details
#' TODO: Add information about approach.
#' TODO: Some additional details about the returned object
#'
#' @return data.frame. Contains the estimated Shapley values for the test data. Note that
#' the dimensions of the data.frame equals \code{n x (p+1)}, where \code{n} equals the number
#' of test observations, and \code{p} equals the total number of features.
#'
#' @export
#'
#' @author Camilla Lingjaerde
explain <- function(x, explainer, approach, prediction_zero, ...) {

  # Check input for x
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Check input for approach
  if (!(is.vector(approach) &&
        is.atomic(approach) &&
        (length(approach) == 1 | length(approach) == ncol(x)) &&
        all(is.element(approach, c("empirical", "gaussian", "copula"))))
  ) {
    stop(
      paste(
        "It seems that you passed a non-valid value for approach.",
        "It should be either 'empirical', 'gaussian', 'copula' or",
        "a vector of length=ncol(x) with only the above characters."
      )
    )
  }

  # Check that x contains correct variables
  explainer$p <- predict_model(explainer$model, head(x, 1))
  explainer$p <- NULL

  if (length(approach) > 1) {
    class(x) <- "combined"
  } else {
    class(x) <- approach
  }

  UseMethod("explain", x)
}

#' @param type Character. Should be equal to either \code{"independence"},
#' \code{"fixed_sigma"}, \code{"AICc_each_k"} or \code{"AICc_full"}.
#'
#' @param fixed_sigma_vec Vector or numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='fixed_sigma'}. The bandwidth to use. Default value \code{0.1}
#'
#' @param n_samples_aicc Positive integer. Only applicable when
#' \code{approach='empirical'} and \code{type='AICc_each_k'} or
#' \code{type='AICc_full'}. Number of samples to consider in AICc optimization.
#'
#' @param eval_max_aicc Positive integer. Only applicable when \code{approach='empirical'}
#' and \code{type='AICc_each_k'} or \code{type='AICc_full'}. Maximum number of iterations when
#' optimizing the AICc.
#'
#' @param start_aicc Numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='AICc_each_k'} or \code{type='AICc_full'}. Starting value when optimizing the AICc.
#'
#' @param w_threshold Positive integer between 0 and 1.
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero,
                              type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              n_samples_aicc = 1000, eval_max_aicc = 20,
                              start_aicc = 0.1, w_threshold = 0.95, ...) {

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(x)
  explainer$approach <- approach
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$n_samples_aicc <- n_samples_aicc
  explainer$eval_max_aicc <- eval_max_aicc
  explainer$start_aicc <- start_aicc
  explainer$w_threshold <- w_threshold

  # Generate data
  dt <- prepare_data(explainer, ...)
  if (!is.null(explainer$return)) return(dt)

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}

#' @inheritParams explain
#'
#' @param mu Numeric vector. (Optional) Containing the mean of the data generating distribution.
#' If \code{NULL} the expected values are estimated from the data. Note that this is only used
#' when \code{approach = "gaussian"}.
#'
#' @param cov_mat Numeric matrix. (Optional) Containing the covariance matrix of the data
#' generating distribution. \code{NULL} means it is estimated from the data if needed
#' (in the Gaussian approach).
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, ...) {

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(x)
  explainer$approach <- approach

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    explainer$mu <- unname(colMeans(explainer$x_train))
  } else {
    explainer$mu <- mu
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
  dt <- prepare_data(explainer, ...)
  if (!is.null(explainer$return)) return(dt)

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}

#' @rdname explain
#' @name explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, ...) {

  # Setup
  explainer$x_test <- as.matrix(x)
  explainer$approach <- approach

  # Prepare transformed data
  x_train <- apply(
    X = explainer$x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  x_test_gaussian <- apply(
    X = rbind(explainer$x_test, explainer$x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(explainer$x_test)
  )

  if (is.null(dim(x_test_gaussian))) {
    x_test_gaussian <- t(as.matrix(x_test_gaussian))
  }

  explainer$mu <- rep(0, ncol(explainer$x_train))
  cov_mat <- stats::cov(x_train) # Gaussian transformed cov. mat
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }
  # Generate data
  dt <- prepare_data(explainer, x_test_gaussian = x_test_gaussian, ...)
  if (!is.null(explainer$return)) return(dt)

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}

#' @rdname explain
#' @export
explain.combined <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, ...) {

  # Get indices of combinations
  l <- get_list_approaches(explainer$X$nfeatures, approach)
  explainer$return <- TRUE
  explainer$x_test <- as.matrix(x)

  dt_l <- list()
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach = names(l)[i], prediction_zero, index_features = l[[i]], ...)
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE)

  r <- prediction(dt, prediction_zero, explainer)

  return(r)

}

#' @keywords internal
get_list_approaches <- function(n_features, approach) {

  l <- list()
  approach[length(approach)] <- approach[length(approach) - 1]

  x <- which(approach == "empirical")
  if (length(x) > 0) {
    if (approach[1] == "empirical") x <- c(0, x)
    l$empirical <- which(n_features %in% x)
  }

  x <- which(approach == "gaussian")
  if (length(x) > 0) {
    if (approach[1] == "gaussian") x <- c(0, x)
    l$gaussian <- which(n_features %in% x)
  }

  x <- which(approach == "copula")
  if (length(x) > 0) {
    if (approach[1] == "copula") x <- c(0, x)
    l$copula <- which(n_features %in% x)

  }
  return(l)
}
