#' Explain the output of machine learning models with more accurately estimated Shapley values
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
#' @param prediction_zero Numeric. The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param ... Additional arguments passed to \code{\link{prepare_data}}
#'
#' @details The most important thing to notice is that \code{shapr} has implemented three different
#' approaches for estimating the conditional distributions of the data, namely \code{"empirical"},
#' \code{"gaussian"} and \code{"copula"}.
#'
#' In addition to this the user will also have the option of combining the three approaches.
#' E.g. if you're in a situation where you have trained a model the consists of 10 features,
#' and you'd like to use the \code{"gaussian"} approach when you condition on a single feature,
#' the \code{"empirical"} approach if you condition on 2-5 features, and \code{"copula"} version
#' if you condition on more than 5 features this can be done by simply passing
#' \code{approach = c("gaussian", rep("empirical", 4), rep("copula", 5))}. If
#' \code{"approach[i]" = "gaussian"} it means that you'd like to use the \code{"gaussian"} approach
#' when conditioning on \code{i} features.
#'
#' @return Object of class \code{c("shapr", "list")}. Contains the following items:
#' \describe{
#'   \item{dt}{data.table}
#'   \item{model}{Model object}
#'   \item{p}{Numeric vector}
#'   \item{x_test}{data.table}
#' }
#'
#' Note that the returned items \code{model}, \code{p} and \code{x_test} are mostly added due
#' to the implementation of \code{plot.shapr}. If you only want to look at the numerical results
#' it is sufficient to focus on \code{dt}. \code{dt} is a data.table where the number of rows equals
#' the number of observations you'd like to explain, and the number of columns equals \code{m +1},
#' where \code{m} equals the total number of features in your model.
#'
#' If \code{dt[i, j + 1] > 0} it indicates that the j-th feature increased the prediction for
#' the i-th observation. Likewise, if \code{dt[i, j + 1] < 0} it indicates that the j-th feature
#' decreased the prediction for the i-th observation. The magnitude of the value is also important
#' to notice. E.g. if \code{dt[i, k + 1]} and \code{dt[i, j + 1]} are greater than \code{0},
#' where \code{j != k}, and \code{dt[i, k + 1]} > \code{dt[i, j + 1]} this indicates that feature
#' \code{j} and \code{k} both increased the value of the prediction, but that the effect of the k-th
#' feature was larger than the j-th feature.
#'
#' The first column in \code{dt}, called `none`, is the prediction value not assigned to any of the features
#' (\ifelse{html}{\eqn{\phi}\out{<sub>0</sub>}}{\eqn{\phi_0}}).
#' It's equal for all observations and set by the user through the argument \code{prediction_zero}.
#' In theory this value should be the expected prediction without conditioning on any features.
#' Typically we set this value equal to the mean of the response variable in our training data, but other choices
#' such as the mean of the predictions in the training data are also reasonable.
#'
#' @export
#'
#' @author Camilla Lingjaerde, Nikolai Sellereite
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#' # Load example data
#' data("Boston", package = "MASS")
#'
#' # Split data into test- and training data
#' x_train <- head(Boston, -3)
#' x_test <- tail(Boston, 3)
#'
#' # Fit a linear model
#' model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#' # Create an explainer object
#' explainer <- shapr(x_train, model)
#'
#' # Explain predictions
#' p <- mean(x_train$medv)
#'
#' # Empirical approach
#' explain1 <- explain(x_test, explainer, approach = "empirical", prediction_zero = p, n_samples = 1e2)
#'
#' # Gaussian approach
#' explain2 <- explain(x_test, explainer, approach = "gaussian", prediction_zero = p, n_samples = 1e2)
#'
#' # Gaussian copula approach
#' explain3 <- explain(x_test, explainer, approach = "copula", prediction_zero = p, n_samples = 1e2)
#'
#' # Combined approach
#' approach <- c("gaussian", "gaussian", "empirical", "empirical")
#' explain4 <- explain(x_test, explainer, approach = approach, prediction_zero = p, n_samples = 1e2)
#'
#' # Print the Shapley values
#' print(explain1$dt)
#'
#' # Plot the results
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' plot(explain1)
#' }
#' }
explain <- function(x, explainer, approach, prediction_zero, ...) {

  # Check input for x
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Check input for approach
  if (!(is.vector(approach) &&
        is.atomic(approach) &&
        (length(approach) == 1 | length(approach) == length(explainer$feature_labels)) &&
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
  if (!all(explainer$feature_labels %in% colnames(x))) {
    stop(
      paste0(
        "\nThe test data, x, does not contain all features necessary for\n",
        "generating predictions. Please modify x so that all labels given\n",
        "by explainer$feature_labels is present in colnames(x)."
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

#' @param type Character. Should be equal to either \code{"independence"},
#' \code{"fixed_sigma"}, \code{"AICc_each_k"} or \code{"AICc_full"}.
#'
#' @param fixed_sigma_vec Numeric. Represents the kernel bandwidth. Note that this argument is only
#' applicable when \code{approach = "empirical"}, and \code{type = "fixed_sigma"}
#'
#' @param n_samples_aicc Positive integer. Number of samples to consider in AICc optimization.
#' Note that this argument is only applicable when \code{approach = "empirical"}, and \code{type}
#' is either equal to \code{"AICc_each_k"} or \code{"AICc_full"}
#'
#' @param eval_max_aicc Positive integer. Maximum number of iterations when
#' optimizing the AICc. Note that this argument is only applicable when
#' \code{approach = "empirical"}, and \code{type} is either equal to
#' \code{"AICc_each_k"} or \code{"AICc_full"}
#'
#' @param start_aicc Numeric. Start value of \code{sigma} when optimizing the AICc. Note that this argument
#' is only applicable when \code{approach = "empirical"}, and \code{type} is either equal to
#' \code{"AICc_each_k"} or \code{"AICc_full"}
#'
#' @param w_threshold Positive integer between 0 and 1.
#'
#' @rdname explain
#'
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero,
                              type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              n_samples_aicc = 1000, eval_max_aicc = 20,
                              start_aicc = 0.1, w_threshold = 0.95, ...) {

  # Add arguments to explainer object
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)
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

#' @param mu Numeric vector. (Optional) Containing the mean of the data generating distribution.
#' If \code{NULL} the expected values are estimated from the data. Note that this is only used
#' when \code{approach = "gaussian"}.
#'
#' @param cov_mat Numeric matrix. (Optional) Containing the covariance matrix of the data
#' generating distribution. \code{NULL} means it is estimated from the data if needed
#' (in the Gaussian approach).
#'
#' @rdname explain
#'
#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, ...) {

  # Add arguments to explainer object
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)
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
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, ...) {

  # Setup
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)
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
  l <- get_list_approaches(explainer$X$n_features, approach)
  explainer$return <- TRUE
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)

  dt_l <- list()
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach = names(l)[i], prediction_zero, index_features = l[[i]], ...)
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE)

  r <- prediction(dt, prediction_zero, explainer)

  return(r)

}

#' Helper function used in \code{\link{explain.combined}}
#'
#' @param n_features Integer vector. Note that
#' \code{length(n_features) <= 2^m}, where \code{m} equals the number
#' of features.
#' @param approach Character vector of length \code{m}. All elements should be
#' either \code{"empirical"}, \code{"gaussian"} or \code{"copula"}.
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite
#'
#' @return List
#'
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

#' @keywords internal
explainer_x_test <- function(x_test, feature_labels) {

  # Remove variables that were not used for training
  x <- data.table::as.data.table(x_test)
  cnms_remove <- setdiff(colnames(x), feature_labels)
  if (length(cnms_remove) > 0) x[, (cnms_remove) := NULL]
  data.table::setcolorder(x, feature_labels)

  return(as.matrix(x))
}
