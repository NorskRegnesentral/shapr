# Libraries -------------------------------------------------------------------------------------------------------
# library(shapr)
# library(rbenchmark)
library(data.table)
devtools::load_all(".")

# Old R code ------------------------------------------------------------------------------------------------------
## R Old version ---------------------------------------------------------------------------------------------------
#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
prepare_data.copula_old <- function(internal, index_features = NULL, ...) {
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

  for (i in seq_len(n_explain)) {
    cat(sprintf("%d,", i))
    l <- lapply(
      X = features,
      FUN = sample_copula_old,
      n_samples = n_samples,
      mu = copula.mu,
      cov_mat = copula.cov_mat,
      m = n_features,
      x_explain = x_explain0[i, , drop = FALSE],
      x_train = as.matrix(x_train),
      x_explain_gaussian = copula.x_explain_gaussian[i, , drop = FALSE]
    )
    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

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
sample_copula_old <- function(index_given, n_samples, mu, cov_mat, m, x_explain_gaussian, x_train, x_explain) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(index_given) %in% c(0, m)) {
    ret <- matrix(x_explain, ncol = m, nrow = 1)
  } else {
    dependent_ind <- (seq_len(length(mu)))[-index_given]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_explain_gaussian[index_given]
    )

    ret0_z <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    ret0_x <- apply(
      X = rbind(ret0_z, x_train[, dependent_ind, drop = FALSE]),
      MARGIN = 2,
      FUN = inv_gaussian_transform_old,
      n_z = n_samples,
      type = 7
    )

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
inv_gaussian_transform_old <- function(zx, n_z, type) {
  if (n_z >= length(zx)) stop("n_z should be less than length(zx)")
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  x_new <- stats::quantile(x, probs = u, type = type)
  return(as.double(x_new))
}

#' Transforms a sample to standardized normal distribution
#'
#' @param x Numeric vector.The data which should be transformed to a standard normal distribution.
#'
#' @return Numeric vector of length `length(x)`
#'
#' @keywords internal
#' @author Martin Jullum
gaussian_transform_old <- function(x) {
  u <- rank(x) / (length(x) + 1)
  z <- stats::qnorm(u)
  return(z)
}

#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' @param yx Numeric vector. The first `n_y` items is the data that is transformed, and last
#' part is the data with the original transformation.
#' @param n_y Positive integer. Number of elements of `yx` that belongs to the Gaussian data.
#'
#' @return Vector of back-transformed Gaussian data
#'
#' @keywords internal
#' @author Martin Jullum
gaussian_transform_separate_old <- function(yx, n_y) {
  if (n_y >= length(yx)) stop("n_y should be less than length(yx)")
  ind <- 1:n_y
  x <- yx[-ind]
  tmp <- rank(yx)[ind]
  tmp <- tmp - rank(tmp) + 0.5
  u_y <- tmp / (length(x) + 1)
  z_y <- stats::qnorm(u_y)
  return(z_y)
}


## C++ arma version -------------------------------------------------------------------------------------------------
#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.copula_cpp_arma <- function(internal, index_features, ...) {
  # Extract used variables
  S <- internal$objects$S[index_features, , drop = FALSE]
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  n_combinations_now <- length(index_features)
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian)

  # TODO: Note that `as.matrix` is not needed for `copula.x_explain_gaussian_mat` as it is already defined as a matrix
  # in `setup_approach.copula`, however, it seems that Martin plans to make it into a data.table, thus, I include
  # `as.matrix` as future safety. DISCUSS WITH MARTIN WHAT HIS PLANS ARE!

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
  # and then transforming them back to the original scale using the inverse Gaussian transform in C++.
  # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
  dt <- prepare_data_copula_cpp_arma(
    MC_samples_mat = MC_samples_mat,
    x_explain_mat = x_explain_mat,
    x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
    x_train_mat = x_train_mat,
    S = S,
    mu = copula.mu,
    cov_mat = copula.cov_mat
  )

  # Reshape `dt` to a 2D array of dimension (n_samples * n_explain * n_coalitions, n_features).
  dim(dt) <- c(n_combinations_now * n_explain * n_samples, n_features)

  # Convert to a data.table and add extra identification columns
  dt <- data.table::as.data.table(dt)
  data.table::setnames(dt, feature_names)
  dt[, id_combination := rep(seq_len(nrow(S)), each = n_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_samples, times = nrow(S))]
  dt[, w := 1 / n_samples]
  dt[, id_combination := index_features[id_combination]]
  data.table::setcolorder(dt, c("id_combination", "id", feature_names))

  return(dt)
}





## C++ and R version  ----------------------------------------------------------------------------------------------
#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.copula_cpp_and_R <- function(internal, index_features, ...) {
  # Extract used variables
  S <- internal$objects$S[index_features, , drop = FALSE]
  feature_names <- internal$parameters$feature_names
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  n_combinations_now <- length(index_features)
  x_train_mat <- as.matrix(internal$data$x_train)
  x_explain_mat <- as.matrix(internal$data$x_explain)
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian)

  # TODO: Note that `as.matrix` is not needed for `copula.x_explain_gaussian_mat` as it is already defined as a matrix
  # in `setup_approach.copula`, however, it seems that Martin plans to make it into a data.table, thus, I include
  # `as.matrix` as future safety. DISCUSS WITH MARTIN WHAT HIS PLANS ARE!

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
  # and then transforming them back to the original scale using the inverse Gaussian transform in C++.
  # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
  dt <- prepare_data_copula_cpp_and_R(
    MC_samples_mat = MC_samples_mat,
    x_explain_mat = x_explain_mat,
    x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
    x_train_mat = x_train_mat,
    S = S,
    mu = copula.mu,
    cov_mat = copula.cov_mat
  )

  # Reshape `dt` to a 2D array of dimension (n_samples * n_explain * n_coalitions, n_features).
  dim(dt) <- c(n_combinations_now * n_explain * n_samples, n_features)

  # Convert to a data.table and add extra identification columns
  dt <- data.table::as.data.table(dt)
  data.table::setnames(dt, feature_names)
  dt[, id_combination := rep(seq_len(nrow(S)), each = n_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_samples, times = nrow(S))]
  dt[, w := 1 / n_samples]
  dt[, id_combination := index_features[id_combination]]
  data.table::setcolorder(dt, c("id_combination", "id", feature_names))

  return(dt)
}

#' Transform data using the inverse Gaussian transformation.
#'
#' @details This function is called from `prepare_data_copula_cpp()` as the this was faster
#'
#' @param z Matrix. The data are the Gaussian Monte Carlos samples to transform.
#' @param x Matrix. The data with the original transformation. Used to conduct the transformation of `z`.
#'
#' @return Matrix of the same dimension as `z`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
inv_gaussian_transform_R <- function(z, x) {
  u <- stats::pnorm(z)
  x_new <- sapply(seq_len(ncol(u)), function(idx) quantile.type7(x[, idx], probs = u[, idx]))
  return(x_new)
}

#' Compute the quantiles using quantile type seven
#'
#' @details Using quantile type number 7 from stats::quantile.
#'
#' @param x numeric vector whose sample quantiles are wanted.
#' @param probs numeric vector of probabilities with values between zero and one.
#'
#' @return A vector of length length(`probs`) is returned.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
quantile.type7 <- function(x, probs) {
  n <- length(x)
  probs <- pmax(0, pmin(1, probs)) # allow for slight overshoot
  index <- 1 + (n - 1) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  x <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]
  i <- which(index > lo)
  h <- (index - lo)[i]
  qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
  return(qs)
}


# Old C++ code ----------------------------------------------------------------------------------------------------
sourceCpp(
  code = '
// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Transforms new data to a standardized normal distribution
//
// @details The function uses `arma::quantile(...)` which corresponds to Rs `stats::quantile(..., type = 5)`.
//
// @param z arma::mat. The data are the Gaussian Monte Carlos samples to transform.
// @param x arma::mat. The data with the original transformation. Used to conduct the transformation of `z`.
//
// @return arma::mat of the same dimension as `z`
//
// @keywords internal
// @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::mat inv_gaussian_transform_cpp_arma(arma::mat z, arma::mat x) {
  int n_features = z.n_cols;
  int n_samples = z.n_rows;
  arma::mat z_new(n_samples, n_features);
  arma::mat u = arma::normcdf(z);
  for (int feature_idx = 0; feature_idx < n_features; feature_idx++) {
    z_new.col(feature_idx) = arma::quantile(x.col(feature_idx), u.col(feature_idx));
  }
  return z_new;
}

// Generate (Gaussian) Copula MC samples
//
// @param MC_samples_mat arma::mat. Matrix of dimension (`n_samples`, `n_features`) containing samples from the
// univariate standard normal.
// @param x_explain_mat arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing the observations
// to explain on the original scale.
// @param x_explain_gaussian_mat arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing the
// observations to explain after being transformed using the Gaussian transform, i.e., the samples have been
// transformed to a standardized normal distribution.
// @param x_train_mat arma::mat. Matrix of dimension (`n_train`, `n_features`) containing the training observations.
// @param S arma::mat. Matrix of dimension (`n_combinations`, `n_features`) containing binary representations of
// the used coalitions. S cannot contain the empty or grand coalition, i.e., a row containing only zeros or ones.
// This is not a problem internally in shapr as the empty and grand coalitions treated differently.
// @param mu arma::vec. Vector of length `n_features` containing the mean of each feature after being transformed
// using the Gaussian transform, i.e., the samples have been transformed to a standardized normal distribution.
// @param cov_mat arma::mat. Matrix of dimension (`n_features`, `n_features`) containing the pairwise covariance
// between all pairs of features after being transformed using the Gaussian transform, i.e., the samples have been
// transformed to a standardized normal distribution.
//
// @return An arma::cube/3D array of dimension (`n_samples`, `n_explain` * `n_coalitions`, `n_features`), where
// the columns (_,j,_) are matrices of dimension (`n_samples`, `n_features`) containing the conditional Gaussian
// copula MC samples for each explicand and coalition on the original scale.
//
// @export
// @keywords internal
// @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::cube prepare_data_copula_cpp_arma(arma::mat MC_samples_mat,
                                       arma::mat x_explain_mat,
                                       arma::mat x_explain_gaussian_mat,
                                       arma::mat x_train_mat,
                                       arma::mat S,
                                       arma::vec mu,
                                       arma::mat cov_mat) {

  int n_explain = x_explain_mat.n_rows;
  int n_samples = MC_samples_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;
  int n_coalitions = S.n_rows;

  // Initialize auxiliary matrix and result cube
  arma::mat aux_mat(n_samples, n_features);
  arma::cube result_cube(n_samples, n_explain*n_coalitions, n_features);

  // Iterate over the coalitions
  for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

    // Get current coalition S and the indices of the features in coalition S and mask Sbar
    arma::mat S_now = S.row(S_ind);
    arma::uvec S_now_idx = arma::find(S_now > 0.5);
    arma::uvec Sbar_now_idx = arma::find(S_now < 0.5);

    // Extract the features we condition on, both on the original scale and the Gaussian transformed values.
    arma::mat x_S_star = x_explain_mat.cols(S_now_idx);
    arma::mat x_S_star_gaussian = x_explain_gaussian_mat.cols(S_now_idx);

    // Extract the mean values of the Gaussian transformed features in the two sets
    arma::vec mu_S = mu.elem(S_now_idx);
    arma::vec mu_Sbar = mu.elem(Sbar_now_idx);

    // Extract the relevant parts of the Gaussian transformed covariance matrix
    arma::mat cov_mat_SS = cov_mat.submat(S_now_idx, S_now_idx);
    arma::mat cov_mat_SSbar = cov_mat.submat(S_now_idx, Sbar_now_idx);
    arma::mat cov_mat_SbarS = cov_mat.submat(Sbar_now_idx, S_now_idx);
    arma::mat cov_mat_SbarSbar = cov_mat.submat(Sbar_now_idx, Sbar_now_idx);

    // Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
    arma::mat cov_mat_SbarS_cov_mat_SS_inv = cov_mat_SbarS * inv(cov_mat_SS);
    arma::mat cond_cov_mat_Sbar_given_S = cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv * cov_mat_SSbar;

    // Ensure that the conditional covariance matrix is symmetric
    if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
      cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
    }

    // Compute the conditional mean of Xsbar given Xs = Xs_star_gaussian, i.e., of the Gaussian transformed features
    arma::mat x_Sbar_gaussian_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star_gaussian.each_row() - mu_S.t()).t();
    x_Sbar_gaussian_mean.each_col() += mu_Sbar;

    // Transform the samples to be from N(O, Sigma_{Sbar|S})
    arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

    // Loop over the different explicands and combine the generated values with the values we conditioned on
    for (int idx_now = 0; idx_now < n_explain; idx_now++) {

      // Transform the MC samples to be from N(mu_{Sbar|S}, Sigma_{Sbar|S}) for one coalition and one explicand
      arma::mat MC_samples_mat_now_now =
        MC_samples_mat_now + repmat(trans(x_Sbar_gaussian_mean.col(idx_now)), n_samples, 1);

      // Transform the MC to the original scale using the inverse Gaussian transform
      arma::mat MC_samples_mat_now_now_trans =
        inv_gaussian_transform_cpp_arma(MC_samples_mat_now_now, x_train_mat.cols(Sbar_now_idx));

      // Insert the generate Gaussian copula MC samples and the feature values we condition on into an auxiliary matrix
      aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now_now_trans;
      aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);

      // Insert the auxiliary matrix into the result cube
      result_cube.col(S_ind*n_explain + idx_now) = aux_mat;
    }
  }

  return result_cube;
}

// Generate (Gaussian) Copula MC samples
//
// @param MC_samples_mat arma::mat. Matrix of dimension (`n_samples`, `n_features`) containing samples from the
// univariate standard normal.
// @param x_explain_mat arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing the observations
// to explain on the original scale.
// @param x_explain_gaussian_mat arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing the
// observations to explain after being transformed using the Gaussian transform, i.e., the samples have been
// transformed to a standardized normal distribution.
// @param x_train_mat arma::mat. Matrix of dimension (`n_train`, `n_features`) containing the training observations.
// @param S arma::mat. Matrix of dimension (`n_combinations`, `n_features`) containing binary representations of
// the used coalitions. S cannot contain the empty or grand coalition, i.e., a row containing only zeros or ones.
// This is not a problem internally in shapr as the empty and grand coalitions treated differently.
// @param mu arma::vec. Vector of length `n_features` containing the mean of each feature after being transformed
// using the Gaussian transform, i.e., the samples have been transformed to a standardized normal distribution.
// @param cov_mat arma::mat. Matrix of dimension (`n_features`, `n_features`) containing the pairwise covariance
// between all pairs of features after being transformed using the Gaussian transform, i.e., the samples have been
// transformed to a standardized normal distribution.
//
// @return An arma::cube/3D array of dimension (`n_samples`, `n_explain` * `n_coalitions`, `n_features`), where
// the columns (_,j,_) are matrices of dimension (`n_samples`, `n_features`) containing the conditional Gaussian
// copula MC samples for each explicand and coalition on the original scale.
//
// @export
// @keywords internal
// @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::cube prepare_data_copula_cpp_and_R(arma::mat MC_samples_mat,
                                         arma::mat x_explain_mat,
                                         arma::mat x_explain_gaussian_mat,
                                         arma::mat x_train_mat,
                                         arma::mat S,
                                         arma::vec mu,
                                         arma::mat cov_mat) {

  int n_explain = x_explain_mat.n_rows;
  int n_samples = MC_samples_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;
  int n_coalitions = S.n_rows;

  // Get the R functions for computing the inverse gaussian transform
  Rcpp::Function inv_gaussian_transform_R("inv_gaussian_transform_R");

  // Initialize auxiliary matrix and result cube
  arma::mat aux_mat(n_samples, n_features);
  arma::cube result_cube(n_samples, n_explain*n_coalitions, n_features);

  // Iterate over the coalitions
  for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

    // Get current coalition S and the indices of the features in coalition S and mask Sbar
    arma::mat S_now = S.row(S_ind);
    arma::uvec S_now_idx = arma::find(S_now > 0.5);
    arma::uvec Sbar_now_idx = arma::find(S_now < 0.5);

    // Extract the features we condition on, both on the original scale and the Gaussian transformed values.
    arma::mat x_S_star = x_explain_mat.cols(S_now_idx);
    arma::mat x_S_star_gaussian = x_explain_gaussian_mat.cols(S_now_idx);

    // Extract the mean values of the Gaussian transformed features in the two sets
    arma::vec mu_S = mu.elem(S_now_idx);
    arma::vec mu_Sbar = mu.elem(Sbar_now_idx);

    // Extract the relevant parts of the Gaussian transformed covariance matrix
    arma::mat cov_mat_SS = cov_mat.submat(S_now_idx, S_now_idx);
    arma::mat cov_mat_SSbar = cov_mat.submat(S_now_idx, Sbar_now_idx);
    arma::mat cov_mat_SbarS = cov_mat.submat(Sbar_now_idx, S_now_idx);
    arma::mat cov_mat_SbarSbar = cov_mat.submat(Sbar_now_idx, Sbar_now_idx);

    // Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
    arma::mat cov_mat_SbarS_cov_mat_SS_inv = cov_mat_SbarS * inv(cov_mat_SS);
    arma::mat cond_cov_mat_Sbar_given_S = cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv * cov_mat_SSbar;

    // Ensure that the conditional covariance matrix is symmetric
    if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
      cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
    }

    // Compute the conditional mean of Xsbar given Xs = Xs_star_gaussian, i.e., of the Gaussian transformed features
    arma::mat x_Sbar_gaussian_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star_gaussian.each_row() - mu_S.t()).t();
    x_Sbar_gaussian_mean.each_col() += mu_Sbar;

    // Transform the samples to be from N(O, Sigma_{Sbar|S})
    arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

    // Loop over the different explicands and combine the generated values with the values we conditioned on
    for (int idx_now = 0; idx_now < n_explain; idx_now++) {

      // Transform the MC samples to be from N(mu_{Sbar|S}, Sigma_{Sbar|S}) for one coalition and one explicand
      arma::mat MC_samples_mat_now_now =
        MC_samples_mat_now + repmat(trans(x_Sbar_gaussian_mean.col(idx_now)), n_samples, 1);

      arma::mat x_train_mat_now = x_train_mat.cols(Sbar_now_idx);
      //arma::mat x_train_mat_now =  arma::normcdf(x_train_mat.cols(Sbar_now_idx));

      // Transform the MC to the original scale using the inverse Gaussian transform
      arma::mat MC_samples_mat_now_now_trans =
        Rcpp::as<arma::mat>(inv_gaussian_transform_R(Rcpp::wrap(MC_samples_mat_now_now),
                                                     Rcpp::wrap(x_train_mat_now)));

      // Insert the generate Gaussian copula MC samples and the feature values we condition on into an auxiliary matrix
      aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now_now_trans;
      aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);

      // Insert the auxiliary matrix into the result cube
      result_cube.col(S_ind*n_explain + idx_now) = aux_mat;
    }
  }

  return result_cube;
}
')










# Setup -----------------------------------------------------------------------------------------------------------
{
  n_samples <- 1000
  n_train <- 1000
  n_test <- 20
  M <- 8
  rho <- 0.5
  betas <- c(0, rep(1, M))

  # We use the Gaussian copula approach
  approach <- "copula"

  # Mean of the multivariate Gaussian distribution
  mu <- rep(0, times = M)
  mu <- seq(M)

  # Create the covariance matrix
  sigma <- matrix(rho, ncol = M, nrow = M) # Old
  for (i in seq(1, M - 1)) {
    for (j in seq(i + 1, M)) {
      sigma[i, j] <- sigma[j, i] <- rho^abs(i - j)
    }
  }
  diag(sigma) <- 1

  # Set seed for reproducibility
  seed_setup <- 1996
  set.seed(seed_setup)

  # Make Gaussian data
  data_train <- data.table(mvtnorm::rmvnorm(n = n_train, mean = mu, sigma = sigma))
  data_test <- data.table(mvtnorm::rmvnorm(n = n_test, mean = mu, sigma = sigma))
  colnames(data_train) <- paste("X", seq(M), sep = "")
  colnames(data_test) <- paste("X", seq(M), sep = "")

  # Make the response
  response_train <- as.vector(cbind(1, as.matrix(data_train)) %*% betas)
  response_test <- as.vector(cbind(1, as.matrix(data_test)) %*% betas)

  # Put together the data
  data_train_with_response <- copy(data_train)[, y := response_train]
  data_test_with_response <- copy(data_test)[, y := response_test]

  # Fit a LM model
  predictive_model <- lm(y ~ ., data = data_train_with_response)

  # Get the prediction zero, i.e., the phi0 Shapley value.
  prediction_zero <- mean(response_train)

  model <- predictive_model
  x_explain <- data_test
  x_train <- data_train
  keep_samp_for_vS <- FALSE
  predict_model <- NULL
  get_model_specs <- NULL
  timing <- TRUE
  n_combinations <- NULL
  group <- NULL
  feature_specs <- shapr:::get_feature_specs(get_model_specs, model)
  n_batches <- 1
  seed <- 1

  internal <- setup(
    x_train = x_train,
    x_explain = x_explain,
    approach = approach,
    prediction_zero = prediction_zero,
    n_combinations = n_combinations,
    group = group,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    feature_specs = feature_specs,
    keep_samp_for_vS = keep_samp_for_vS,
    predict_model = predict_model,
    get_model_specs = get_model_specs,
    timing = timing
  )

  # Gets predict_model (if not passed to explain)
  predict_model <- shapr:::get_predict_model(
    predict_model = predict_model,
    model = model
  )

  # Sets up the Shapley (sampling) framework and prepares the
  # conditional expectation computation for the chosen approach
  # Note: model and predict_model are ONLY used by the AICc-methods of approach empirical to find optimal parameters
  internal <- shapr:::setup_computation(internal, model, predict_model)
}


# Compare shapr compile and rcpp compile --------------------------------------------------------------------------
look_at_coalitions <- seq(1, 2^M - 2)
index_features = internal$objects$S_batch$`1`[look_at_coalitions]
S <- internal$objects$S[index_features, , drop = FALSE]
feature_names <- internal$parameters$feature_names
n_explain <- internal$parameters$n_explain
n_samples <- internal$parameters$n_samples
n_features <- internal$parameters$n_features
n_combinations_now <- length(index_features)
x_train_mat <- as.matrix(internal$data$x_train)
x_explain_mat <- as.matrix(internal$data$x_explain)
copula.mu <- internal$parameters$copula.mu
copula.cov_mat <- internal$parameters$copula.cov_mat
copula.x_explain_gaussian_mat <- as.matrix(internal$data$copula.x_explain_gaussian)

# Generate the MC samples from N(0, 1)
MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

# Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
# and then transforming them back to the original scale using the inverse Gaussian transform in C++.
# The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
time_1 = system.time({dt_1 <- prepare_data_copula_cpp(
  MC_samples_mat = MC_samples_mat,
  x_explain_mat = x_explain_mat,
  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
  x_train_mat = x_train_mat,
  S = S,
  mu = copula.mu,
  cov_mat = copula.cov_mat
)})
time_1

time_2 = system.time({dt_2 <- shapr:::prepare_data_copula_cpp(
  MC_samples_mat = MC_samples_mat,
  x_explain_mat = x_explain_mat,
  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
  x_train_mat = x_train_mat,
  S = S,
  mu = copula.mu,
  cov_mat = copula.cov_mat
)})
time_2

time_2andhalf = system.time({dt_2andhalf <-
  .Call(`_shapr_prepare_data_copula_cpp`,
        MC_samples_mat = MC_samples_mat,
        x_explain_mat = x_explain_mat,
        x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
        x_train_mat = x_train_mat,
        S = S,
        mu = copula.mu,
        cov_mat = copula.cov_mat
  )})
time_2andhalf

# Rcpp::compileAttributes(pkgdir = ".", verbose = TRUE)
Rcpp::sourceCpp("src/Copula.cpp")
time_3 = system.time({dt_3 <- prepare_data_copula_cpp(
  MC_samples_mat = MC_samples_mat,
  x_explain_mat = x_explain_mat,
  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
  x_train_mat = x_train_mat,
  S = S,
  mu = copula.mu,
  cov_mat = copula.cov_mat
)})
time_3

time_4 = system.time({dt_4 <- shapr::prepare_data_copula_cpp(
  MC_samples_mat = MC_samples_mat,
  x_explain_mat = x_explain_mat,
  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
  x_train_mat = x_train_mat,
  S = S,
  mu = copula.mu,
  cov_mat = copula.cov_mat
)})

rbind(time_1, time_2, time_3, time_4)
all.equal(dt_1, dt_2)
all.equal(dt_2, dt_3)
all.equal(dt_3, dt_4)

# Compare prepare_data.copula ----------------------------------------------------------------------------------------
set.seed(123)

# Recall that old version iterate over the observations and then the coalitions.
# While the new version iterate over the coalitions and then the observations.
# The latter lets us reuse the computed conditional distributions for all observations.
look_at_coalitions <- seq(1, 2^M - 2)
# look_at_coalitions <- seq(1, 2^M - 2, 10)
# look_at_coalitions <- seq(1, 2^M - 2, 25)

# The old R code
time_only_R <- system.time({
  res_only_R <- prepare_data.copula_old(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions]
  )
})
time_only_R

# The C++ code with my own quantile function
time_only_cpp <- system.time({
  res_only_cpp <- prepare_data.copula(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions]
  )
})
data.table::setorderv(res_only_cpp, c("id", "id_combination"))
time_only_cpp

# The C++ code with quantile functions from arma
time_only_cpp_arma <- system.time({
  res_only_cpp_arma <- prepare_data.copula_cpp_arma(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions]
  )
})
data.table::setorderv(res_only_cpp_arma, c("id", "id_combination"))
time_only_cpp_arma

# The new C++ code with quantile from R
time_cpp_and_R <- system.time({
  res_cpp_and_R <- prepare_data.copula_cpp_and_R(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions]
  )
})
data.table::setorderv(res_cpp_and_R, c("id", "id_combination"))
time_cpp_and_R

# Create a table of the times. Less is better
times <- rbind(
  time_only_R,
  time_only_cpp,
  time_only_cpp_arma,
  time_cpp_and_R
)
times

# TIMES for all coalitions (254), n_samples <- 1000, n_train <- 1000, n_test <- 20, M <- 8

#                    user.self sys.self elapsed user.child sys.child
# time_only_R           67.050    2.587  72.357      0.011     0.018
# time_only_cpp          4.588    0.406   5.218      0.000     0.000
# time_only_cpp_arma    23.853    0.663  25.391      0.000     0.000
# time_cpp_and_R         7.430    1.346   9.086      0.000     0.000


# Relative speedup of new method
times_relative <- t(sapply(seq_len(nrow(times)), function(idx) times[1, ] / times[idx, ]))
rownames(times_relative) <- paste0(rownames(times), "_rel")
times_relative

# RELATIVE TIMES for all coalitions, n_samples <- 1000, n_train <- 1000, n_test <- 20, M <- 8

#                        user.self sys.self elapsed user.child sys.child
# time_only_R_rel           1.0000   1.0000  1.0000          1         1
# time_only_cpp_rel        14.6142   6.3719 13.8668        Inf       Inf
# time_only_cpp_arma_rel    2.8110   3.9020  2.8497        Inf       Inf
# time_cpp_and_R_rel        9.0242   1.9220  7.9636        Inf       Inf

# Aggregate the MC sample values for each explicand and combination
res_only_R <- res_only_R[, w := NULL]
res_only_cpp <- res_only_cpp[, w := NULL]
res_only_cpp_arma <- res_only_cpp_arma[, w := NULL]
res_cpp_and_R <- res_cpp_and_R[, w := NULL]
res_only_R_agr <- res_only_R[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_agr <- res_only_cpp[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_arma_agr <- res_only_cpp_arma[, lapply(.SD, mean), by = c("id", "id_combination")]
res_cpp_and_R_agr <- res_cpp_and_R[, lapply(.SD, mean), by = c("id", "id_combination")]

# Difference
res_only_R_agr - res_only_cpp_agr
res_only_R_agr - res_only_cpp_arma_agr
res_only_R_agr - res_cpp_and_R_agr

# Max absolute difference
max(abs(res_only_R_agr - res_only_cpp_agr))
max(abs(res_only_R_agr - res_only_cpp_arma_agr))
max(abs(res_only_R_agr - res_cpp_and_R_agr))

# Max absolute relative difference
max(abs(res_only_R_agr - res_only_cpp_agr) / res_only_cpp_agr)
max(abs(res_only_R_agr - res_only_cpp_arma_agr) / res_only_cpp_arma_agr)
max(abs(res_only_R_agr - res_cpp_and_R_agr) / res_cpp_and_R_agr)

# Compare gaussian_transform --------------------------------------------------------------------------------------
set.seed(123)
x_temp_rows = 100000
x_temp_cols = 20
x_temp = matrix(rnorm(x_temp_rows*x_temp_cols), x_temp_rows, x_temp_cols)

# Compare for equal values
system.time({gaussian_transform_R = apply(X = x_temp, MARGIN = 2, FUN = gaussian_transform_old)})
system.time({gaussian_transform_cpp = gaussian_transform_cpp(x_temp)})
system.time({gaussian_transform_cpp = shapr:::gaussian_transform_cpp(x_temp)})
all.equal(gaussian_transform_R, gaussian_transform_cpp) # TRUE

# Compare time (generate new data each time such that the result is not stored in the cache)
set.seed(1234)
gc()
#Rcpp::sourceCpp("src/Copula.cpp") # C++ code is faster when I recompile it? I don't understand.
rbenchmark::benchmark(R = apply(X = matrix(rnorm(x_temp_rows*x_temp_cols), x_temp_rows, x_temp_cols),
                                MARGIN = 2,
                                FUN = gaussian_transform_old),
                      cpp = gaussian_transform_cpp(matrix(rnorm(x_temp_rows*x_temp_cols), x_temp_rows, x_temp_cols)),
                      replications = 100)
#   test replications elapsed relative user.self sys.self user.child sys.child
# 2  cpp          100   1.933    1.000     1.764    0.149          0         0
# 1    R          100   3.152    1.631     2.498    0.511          0         0



# Compare gaussian_transform_separate -------------------------------------------------------------------------
set.seed(123)
x_cols = 10
x_train_rows = 50000
x_explain_rows = 50000
x_train_temp = matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)
x_explain_temp = matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols)
x_explain_train_temp = rbind(x_explain_temp, x_train_temp)

system.time({r = apply(X = rbind(x_explain_temp, x_train_temp),
                        MARGIN = 2,
                        FUN = gaussian_transform_separate_old,
                        n_y = nrow(x_explain_temp))})
system.time({cpp = gaussian_transform_separate_cpp(x_explain_temp, x_train_temp)})
system.time({cpp_shapr = shapr:::gaussian_transform_separate_cpp(x_explain_temp, x_train_temp)})
all.equal(r, cpp)

# gc()
# Rcpp::sourceCpp("src/Copula.cpp") # C++ code is faster when I recompile it? I don't understand.

rbenchmark::benchmark(r = apply(X = rbind(x_explain_temp, x_train_temp),
                                MARGIN = 2,
                                FUN = gaussian_transform_separate_old,
                                n_y = nrow(x_explain_temp)),
                      cpp = gaussian_transform_separate_cpp(x_explain_temp, x_train_temp))
#   test replications elapsed relative user.self sys.self user.child sys.child
# 2  cpp          100   0.238    1.000     0.228    0.006          0         0
# 1    r          100   0.502    2.109     0.432    0.058          0         0

Sys.setenv("PKG_CXXFLAGS"="-O0")
Rcpp::sourceCpp("src/Copula.cpp")
rbenchmark::benchmark(r = apply(X = rbind(matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols),
                                          matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                                MARGIN = 2,
                                FUN = gaussian_transform_separate_old,
                                n_y = nrow(matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols))),
                      cpp = gaussian_transform_separate_cpp(matrix(rnorm(x_explain_rows*x_cols),
                                                                   x_explain_rows,
                                                                   x_cols),
                                      matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      cpp_dir = gaussian_transform_separate_cpp2(matrix(rnorm(x_explain_rows*x_cols),
                                                                   x_explain_rows,
                                                                   x_cols),
                                                            matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      cpp_shapr = shapr:::gaussian_transform_separate_cpp(matrix(rnorm(x_explain_rows*x_cols),
                                                                   x_explain_rows,
                                                                   x_cols),
                                                            matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      cpp_shapr_dir = shapr:::gaussian_transform_separate_cpp2(matrix(rnorm(x_explain_rows*x_cols),
                                                                   x_explain_rows,
                                                                   x_cols),
                                                            matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      cpp2 = .Call(`_shapr_gaussian_transform_separate_cpp`,
                                   matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols),
                                   matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)))

#   test replications elapsed relative user.self sys.self user.child sys.child
# 2  cpp          100   0.361    1.000     0.322    0.025          0         0
# 1    r          100   0.553    1.532     0.496    0.047          0         0


# x_cols = 5, x_train_rows = 10000, x_explain_rows = 10000
#            test replications elapsed relative user.self sys.self user.child sys.child
# 2           cpp          100   2.526    1.000     2.342    0.145          0         0
# 3       cpp_dir          100   2.574    1.019     2.359    0.136          0         0
# 4     cpp_shapr          100   5.314    2.104     4.955    0.173          0         0
# 5 cpp_shapr_dir          100   5.274    2.088     5.020    0.169          0         0
# 6          cpp2          100   5.448    2.157     5.112    0.169          0         0
# 1             r          100   4.791    1.897     3.926    0.756          0         0

# Call `Rcpp::sourceCpp("src/Copula.cpp")` and then run rbenchmark again and then cpp is much faster.
# C++ code is faster when I recompile it? I don't understand.
# Rcpp::sourceCpp("src/Copula.cpp")
