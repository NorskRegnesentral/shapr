#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#' @description TODO: Add a more detailed description
#'
#' @param x A matrix or data.frame. Contains the the features, whose
#' predictions ought to be explained (test data).
#'
#' @param explainer An \code{explainer} object to use for exaplaining the observations.
#' See \code{\link{shapr}}.
#'
#' @param approach Character. Note that \code{1 <= length(approach) <= n_features}, where
#' \code{n_features} where equals the total number of features in the model. All elements should
#' either be \code{gaussian}, \code{copula} or \code{empirical}. See details for more information.
#'
#' @param prediction_zero The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param n_samples Positive integer. Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation.
#'
#' @param seed Positive integer. If \code{NULL} a random seed will be used.
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
        (length(approach) < ncol(x)) &&
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
    class(x) <- "combinded"
  } else {
    class(x) <- approach
  }

  UseMethod("explain", x)
}

#' @param type String or list. Only applicable when \code{approach='empirical'}. If a string, the
#' type of empirical approach to use,  equal to 'independence, 'gaussian' or 'fixed_sigma'. If a
#' list, the elements in the list refers to the rows in \code{x} that ought to be included in
#' each of the empirical approaches.
#'
#' @param fixed_sigma_vec Vector or numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='fixed_sigma'}. The bandwidth to use. Default value \code{0.1}
#'
#' @param AICc_no_samp_per_optim Positive integer. Only applicable when
#' \code{approach='empirical'} and \code{type='AICc_each_k'} or
#' \code{type='AICc_full'}. Number of samples to consider in AICc optimization.
#'
#' @param AIC_optim_max_eval Positive integer. Only applicable when \code{approach='empirical'}
#' and \code{type='AICc_each_k'} or \code{type='AICc_full'}. Numeric. Maximum value when
#' optimizing the AICc.
#'
#' @param AIC_optim_startval Numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='AICc_each_k'} or \code{type='AICc_full'}. Starting value when optimizing the AICc.
#'
#' @param w_threshold Postive integer between 0 and 1.
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

  # Get distance matrix ----------------
  explainer$D <- distance_matrix(
    explainer$x_train,
    x,
    explainer$X$features
  )
  # Generate data
  dt <- prepare_data(explainer, ...)

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

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)
}

#' @rdname explain
#' @export
explain.combined <- function(x, explainer, prediction_zero, approach = NULL,
                             empirical.types = NULL, fixed_sigma_vec = 0.1,
                             AICc_no_samp_per_optim = 1000, AIC_optim_max_eval = 20,
                             AIC_optim_startval = 0.1, w_threshold = 0.95, mu = NULL, cov_mat = NULL, ...) {
  one.obs.only <- is.null(nrow(x))

  if (is.null(approach)) {
    if (one.obs.only) {
      approach <- list("empirical" = 1)
    } else {
      approach <- list("empirical" = 1:nrow(x))
    }
  }
  if (is.null(empirical.types)) {
    if (!one.obs.only) {
      empirical.types <- list("independence" = 1:nrow(x))
    } else {
      empirical.types <- list("independence" = 1)
    }
  }
  dt.final <- NULL
  dt.rownames <- NULL
  if (!all(names(approach) %in% c("empirical", "gaussian", "copula"))) {
    stop("Approach must be 'empirical','gaussian' or 'copula'")
  }
  if ("empirical" %in% names(approach)) {
    if (!all(names(empirical.types) %in% c("independence", "fixed_sigma", "AICc_each_k", "AICc_full"))) {
      stop("Empirical approach must be 'independence','fixed_sigma','AICc_each_k' or 'AICc_full'")
    }
    if ("independence" %in% names(empirical.types)) {
      emp_indep_ind <- empirical.types$independence
      if (one.obs.only) {
        x.emp.indep <- x
      } else {
        x.emp.indep <- x[emp_indep_ind, ]
      }
      dt.indep <- explain.empirical(x.emp.indep, explainer,
        approach = "empirical", type = "independence",
        prediction_zero = prediction_zero, w_threshold = w_threshold
      )
      dt.final <- rbind(dt.final, dt.indep)
      dt.rownames <- c(dt.rownames, emp_indep_ind)
    }
    if ("fixed_sigma" %in% names(empirical.types)) {
      emp_fixed_ind <- empirical.types$fixed_sigma
      if (one.obs.only) {
        x.emp.fixed <- x
      } else {
        x.emp.fixed <- x[emp_fixed_ind, ]
      }
      dt.fixed <- explain.empirical(x.emp.fixed, explainer,
        approach = "empirical", prediction_zero = prediction_zero,
        type = "fixed_sigma", fixed_sigma_vec = fixed_sigma_vec, w_threshold = w_threshold
      )
      dt.final <- rbind(dt.final, dt.fixed)
      dt.rownames <- c(dt.rownames, emp_fixed_ind)
    }
    if ("AICc_each_k" %in% names(empirical.types)) {
      emp_each_ind <- empirical.types$AICc_each_k
      if (one.obs.only) {
        x.emp.each <- x
      } else {
        x.emp.each <- x[emp_each_ind, ]
      }
      dt.AICc_each <- explain.empirical(x.emp.each, explainer,
        approach = "empirical", type = "AICc_each_k", prediction_zero = prediction_zero,
        AICc_no_samp_per_optim = AICc_no_samp_per_optim, AIC_optim_max_eval = AIC_optim_max_eval,
        AIC_optim_startval = AIC_optim_startval, w_threshold = w_threshold
      )
      dt.final <- rbind(dt.final, dt.AICc_each)
      dt.rownames <- c(dt.rownames, emp_each_ind)
    }
    if ("AICc_full" %in% names(empirical.types)) {
      emp_full_ind <- empirical.types$AICc_full
      if (one.obs.only) {
        x.emp.full <- x
      } else {
        x.emp.full <- x[emp_full_ind, ]
      }
      dt.AICc_full <- explain.empirical(x.emp.full, explainer,
        approach = "empirical", prediction_zero = prediction_zero,
        type = "AICc_full", AICc_no_samp_per_optim = AICc_no_samp_per_optim, AIC_optim_max_eval = AIC_optim_max_eval,
        AIC_optim_startval = AIC_optim_startval, w_threshold = w_threshold
      )
      dt.final <- rbind(dt.final, dt.AICc_full)
      dt.rownames <- c(dt.rownames, emp_full_ind)
    }
  }
  if ("gaussian" %in% names(approach)) {
    gaussian_ind <- approach$gaussian
    if (!one.obs.only) {
      x.gauss <- x[gaussian_ind, ]
    } else {
      x.gauss <- x
    }
    dt.gaussian <- explain.gaussian(x.gauss, explainer, approach = "gaussian", prediction_zero = prediction_zero, mu = mu, cov_mat = cov_mat, n_samples = n_samples)
    dt.final <- rbind(dt.final, dt.gaussian)
    dt.rownames <- c(dt.rownames, gaussian_ind)
  }
  if ("copula" %in% names(approach)) {
    copula_ind <- approach$copula
    if (!one.obs.only) {
      x.copula <- x[copula_ind, ]
    } else {
      x.copula <- x
    }
    dt.copula <- explain.copula(x.copula, explainer, approach = "copula", prediction_zero = prediction_zero, n_samples = n_samples)
    dt.final <- rbind(dt.final, dt.copula)
    dt.rownames <- c(dt.rownames, copula_ind)
  }
  if (!one.obs.only) {
    rownames(dt.final) <- dt.rownames
    dt.final <- dt.final[order(dt.rownames), ]
  }
  return(dt.final)
}
