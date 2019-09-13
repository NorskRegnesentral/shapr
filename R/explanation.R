#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#'
#' @param x A matrix or data.frame. Contains the the features, whose
#' predictions ought to be explained (test data).
#'
#' @param explainer An list returned by \code{shapr} whose elements are used to explain the observations.
#' See \code{\link{shapr}}.
#'
#' @param approach Character or List. The method to use when computing the Shapley values. If a list, a combined approach is used where element \code{i} indicates the approach to use when conditioning on \code{i} variables. Note that \code{1 <= length(approach) <= n_features}, where
#' \code{n_features} equals the total number of features in the model. All elements should
#' either be \code{gaussian}, \code{copula} or \code{empirical}. See details for more information.
#'
#' @param prediction_zero The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param n_samples Positive integer. Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation.
#'
#' @param seed Positive integer. If \code{NULL} a random seed will be used. Useful for debugging.
#'
#' @param ... Additional arguments passed to \code{\link{prepare_data}}
#'
#'
#' @return data.frame. Contains the estimated Shapley values for the test data. Note that
#' the dimensions of the data.frame equals \code{ntest x (p+1)}, where \code{ntest} equals the number
#' of test observations, and \code{p} equals the total number of features.
#'
#' @export
#'
#' @author Camilla Lingjaerde
explain <- function(x, explainer, approach='empirical', prediction_zero, ...) {

  # Check input for x
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Check input for approach
  if (!(is.vector(approach) &&
        is.atomic(approach) &&
        (length(approach) <= ncol(x)) &&
        all(is.element(approach, c("empirical", "gaussian", "copula"))))
  ) {
    stop(
      paste(
        "It seems that you passed a non-valid value for approach.",
        "It should be either 'empirical', 'gaussian', 'copula' or",
        "a list."
      )
    )
  }

  if (length(approach) > 1) {
    class(x) <- "combined"
  } else {
    class(x) <- approach
  }

  UseMethod("explain", x)
}

#' @param type String or list. Only applicable when \code{approach='empirical'}. If a string, the
#' type of empirical approach to use,  equal to 'independence, 'gaussian' or 'fixed_sigma' (default). If a
#' list, the elements in the list refers to the rows in \code{x} that ought to be included in
#' each of the empirical approaches.
#'
#' @param fixed_sigma_vec Vector or numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='fixed_sigma'}. The bandwidth to use. Default value \code{0.1}
#'
#' @param AICc_no_samp_per_optim Positive integer. Only applicable when
#' \code{approach='empirical'} and \code{type='AICc_each_k'} or
#' \code{type='AICc_full'}. Number of samples to consider in AICc optimization. Default value \code{1000}.
#'
#' @param AIC_optim_max_eval Positive integer. Only applicable when \code{approach='empirical'}
#' and \code{type='AICc_each_k'} or \code{type='AICc_full'}. Numeric. Maximum value when
#' optimizing the AICc. Default value \code{20}.
#'
#' @param AIC_optim_startval Numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='AICc_each_k'} or \code{type='AICc_full'}. Starting value when optimizing the AICc. Default value \code{0.1}.
#'
#' @param w_threshold Postive integer between 0 and 1. Default value \code{0.95}. We choose the number \code{K} of observations to use so that they account for this fraction of the total weight.
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero,
                              type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              AICc_no_samp_per_optim = 1000, AIC_optim_max_eval = 20,
                              AIC_optim_startval = 0.1, w_threshold = 0.95, ...) {

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(x)
  explainer$approach <- approach
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$AICc_no_samp_per_optim <- AICc_no_samp_per_optim
  explainer$AIC_optim_max_eval <- AIC_optim_max_eval
  explainer$AIC_optim_startval <- AIC_optim_startval
  explainer$w_threshold <- w_threshold

  # Generate data
  dt <- prepare_data(explainer, ...)
  if (!is.null(explainer$return)) return(dt)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)
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
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)
}

#' @rdname explain
#' @name explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, ...) {

  # Setup
  explainer$x_test <- as.matrix(x)
  explainer$x_test <- x
  explainer$approach <- approach

  # Prepare transformed data
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
  if (is.null(dim(x))) {
    x_test <- t(as.matrix(x))
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
  dt <- prepare_data(explainer, x_test = x_test, ...)
  if (!is.null(explainer$return)) return(dt)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)
}

#' @rdname explain
#' @export
explain.combined <- function(x, explainer, prediction_zero, approach = NULL, mu = NULL, cov_mat = NULL, ...) {

  # Get indices of combinations
  l <- get_list_approaches(explainer$X$nfeatures, approach)
  explainer$return <- TRUE
  explainer$x_test <- as.matrix(x)
  dt_e <- dt_g <- dt_c <- NULL

  if (!is.null(l$empirical)) {
    dt_e <- explain(x, explainer, approach = "empirical", prediction_zero, index_features = l$empirical, ...)
  }

  if (!is.null(l$gaussian)) {
    dt_g <- explain(x, explainer, approach = "gaussian", prediction_zero, index_features = l$gaussian, ...)

  }

  if (!is.null(l$copula)) {
    dt_c <- explain(x, explainer, approach = "copula", prediction_zero, index_features = l$copula, ...)
  }

  dt <- data.table::rbindlist(list(dt_e, dt_g, dt_c), use.names = TRUE)

  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)

}

#' @keywords internal
get_list_approaches <- function(n_features, approach) {

  l <- list()

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
