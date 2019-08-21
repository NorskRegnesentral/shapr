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

#' hey
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

  # Prepare data
  dt <- prepare_data(explainer, ...)

  # Predict

  # Process

  # Return

  return(explainer$D)
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
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)
}

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

#' @export
explain.combined <- function(x, explainer, approach, prediction_zero, ...) {

  # Setup
  explainer$n_samples <- n_samples
  explainer$x_test <- x
  explainer$approach <- approach

}

#' @export
prepare_data <- function(x, ...){
  class(x) <- x$approach
  UseMethod("prepare_data", x)
}

#' @export
prepare_data.empirical <- function(x, type = "independence"){

  kernel_metric <- ifelse(type == "independence", type, "gaussian")
  browser()
  # Handle the computation of all training-test weights for ALL combinations here, before looping
  if (kernel_metric == "independence") {

    # Adds random noise to "fake" a distance between observations
    n <- no_wcomb * nrow(x$x_train)
    W_kernel <- array(
      stats::runif(n),
      dim = c(nrow(x$x_train), no_wcomb)
    )
  } else if(kernel_metric == "Gaussian") {

    val <- t(t(-0.5 * x$D) / (x$h_optim_vec)^2)
    W_kernel <- exp(val)
    # To avoid numerical problems for small sigma values, we need to substract some constant from
    # val here. Check if it is possible to do this per column/row of l$D[,i,]
  } else {
    stop("It seems that you've passed a non-valid value when using kernel_metric")
  }

  # Generate permutations of data
  dt <- observation_impute(
    W_kernel = W_kernel,
    S = S[these_wcomb, ],
    Xtrain = Xtrain,
    Xtest = Xtest,
    w_threshold = w_threshold,
    noSamp_MC = noSamp_MC
  )

  return(dt)
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
prepare_data.copula <- function(x, x_test){

  n_xtest <- nrow(x$x_test)
  dt_l <- list()

  for (i in seq(n_xtest)) {

    l <- lapply(
      X = x$X$features,
      FUN = sample_copula,
      noSamp_MC = x$n_samples,
      mu = x$mu,
      Sigma = x$cov_mat,
      p = x$n_features,
      Xtest = x$x_test[i,, drop = FALSE],
      Xtrain = as.matrix(x$x_train),
      Xtest_Gauss_trans = x_test
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "wcomb")
    # dt[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    dt_l[[i]][, w := 1 / x$n_samples]
    dt_l[[i]][, id := i]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

}
