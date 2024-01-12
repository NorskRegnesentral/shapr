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
      x_explain_gaussian = as.matrix(copula.x_explain_gaussian)[i, , drop = FALSE]
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



# C++ with sourceRcpp ---------------------------------------------------------------------------------------------
#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.copula_sourceCpp <- function(internal, index_features, ...) {
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

  # Generate the MC samples from N(0, 1)
  MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

  # Use C++ to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}), for all coalitions and explicands,
  # and then transforming them back to the original scale using the inverse Gaussian transform in C++.
  # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
  dt <- prepare_data_copula_cpp_sourceCpp(
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

Rcpp::sourceCpp(
  code = '
  #include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// Compute the quantiles using quantile type seven
//
// @details Using quantile type number seven from stats::quantile in R.
//
// @param x arma::vec. Numeric vector whose sample quantiles are wanted.
// @param probs arma::vec. Numeric vector of probabilities with values between zero and one.
//
// @return A vector of length `length(probs)` with the quantiles is returned.
//
// @keywords internal
// @author Lars Henry Berge Olsen
// [[Rcpp::export]]
  arma::vec quantile_type7_cpp_sourceCpp(const arma::vec& x, const arma::vec& probs) {
    int n = x.n_elem;
    int m = probs.n_elem;

    // Initialize output quantile vector
    arma::vec qs(m);

    // Calculate indices
    arma::vec index = 1 + (n - 1) * probs;
    arma::vec lo = arma::floor(index);
    arma::vec hi = arma::ceil(index);

    // Sort the data
    arma::vec sorted_x = arma::sort(x);

    // Calculate quantiles using quantile type seven
    for (int i = 0; i < m; ++i) {
      qs(i) = sorted_x(lo(i) - 1);
      if (index(i) > lo(i)) {
        double h = index(i) - lo(i);
        qs(i) = (1 - h) * qs(i) + h * sorted_x(hi(i) - 1);
      }
    }

    return qs;
  }

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
  arma::mat inv_gaussian_transform_cpp_sourceCpp(const arma::mat& z, const arma::mat& x) {
    int n_features = z.n_cols;
    int n_samples = z.n_rows;
    arma::mat z_new(n_samples, n_features);
    arma::mat u = arma::normcdf(z);
    for (int feature_idx = 0; feature_idx < n_features; feature_idx++) {
      z_new.col(feature_idx) = quantile_type7_cpp_sourceCpp(x.col(feature_idx), u.col(feature_idx));
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
  arma::cube prepare_data_copula_cpp_sourceCpp(const arma::mat& MC_samples_mat,
                                     const arma::mat& x_explain_mat,
                                     const arma::mat& x_explain_gaussian_mat,
                                     const arma::mat& x_train_mat,
                                     const arma::mat& S,
                                     const arma::vec& mu,
                                     const arma::mat& cov_mat) {

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
          inv_gaussian_transform_cpp_sourceCpp(MC_samples_mat_now_now, x_train_mat.cols(Sbar_now_idx));

        // Insert the generate Gaussian copula MC samples and the feature values we condition on into an auxiliary matrix
        aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now_now_trans;
        aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);

        // Insert the auxiliary matrix into the result cube
        result_cube.col(S_ind*n_explain + idx_now) = aux_mat;
      }
    }

    return result_cube;
  }

// Transforms a sample to standardized normal distribution
//
// @param x Numeric matrix. The data which should be transformed to a standard normal distribution.
//
// @return Numeric matrix of dimension `dim(x)`
//
// @keywords internal
// @author Lars Henry Berge Olsen
// [[Rcpp::export]]
Rcpp::NumericMatrix gaussian_transform_cpp_sourceCpp(const arma::mat& x) {
  int n_obs = x.n_rows;
  int n_features = x.n_cols;

  // Pre allocate the return matrix
  Rcpp::NumericMatrix x_trans(n_obs, n_features);

  // Iterate over the columns, i.e., the features
  for (int idx_feature = 0; idx_feature < n_features; ++idx_feature) {
    // Compute the rank and transform to standardized normal distribution
    arma::vec rank_now = arma::conv_to<arma::vec>::from(arma::sort_index(arma::sort_index(x.col(idx_feature))));
    Rcpp::NumericVector u = Rcpp::wrap((rank_now + 1) / (n_obs + 1));
    x_trans(Rcpp::_, idx_feature) = Rcpp::qnorm(u);
  }

  return x_trans;
}

// Transforms new data to standardized normal (column-wise) based on other data transformations
//
// @param y arma::mat. A numeric matrix containing the data that is to be transformed.
// @param x arma::mat. A numeric matrix containing the data of the original transformation.
//
// @return An arma::mat matrix of the same dimension as `y` containing the back-transformed Gaussian data.
//
// @keywords internal
// @author Lars Henry Berge Olsen, Martin Jullum
// [[Rcpp::export]]
Rcpp::NumericMatrix gaussian_transform_separate_cpp_sourceCpp(const arma::mat& y, const arma::mat& x) {
  int n_features = x.n_cols;
  int n_y_rows = y.n_rows;
  int n_x_rows = x.n_rows;

  // Pre allocate the return matrix
  Rcpp::NumericMatrix z_y(n_y_rows, n_features);

  // Compute the transformation for each feature at the time
  for (int idx_feature = 0; idx_feature < n_features; ++idx_feature) {
    arma::vec yx_now = arma::join_cols(y.col(idx_feature), x.col(idx_feature));
    arma::vec rank_now_1 = arma::conv_to<arma::vec>::from(arma::sort_index(arma::sort_index(yx_now))).head(n_y_rows);
    arma::vec rank_now_2 = arma::conv_to<arma::vec>::from(arma::sort_index(arma::sort_index(rank_now_1)));
    arma::vec tmp = rank_now_1 - rank_now_2 + 0.5;
    Rcpp::NumericVector u_y = Rcpp::wrap(tmp / (n_x_rows + 1));
    z_y(Rcpp::_, idx_feature) = Rcpp::qnorm(u_y);
  }

  return z_y;
}

  '
)



# Old C++ code ----------------------------------------------------------------------------------------------------
Rcpp::sourceCpp(
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

time_3 = system.time({dt_3 <-
  .Call(`_shapr_prepare_data_copula_cpp`,
        MC_samples_mat = MC_samples_mat,
        x_explain_mat = x_explain_mat,
        x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
        x_train_mat = x_train_mat,
        S = S,
        mu = copula.mu,
        cov_mat = copula.cov_mat
  )})
time_3

Rcpp::sourceCpp("src/Copula.cpp")
time_4 = system.time({dt_4 <- prepare_data_copula_cpp(
  MC_samples_mat = MC_samples_mat,
  x_explain_mat = x_explain_mat,
  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
  x_train_mat = x_train_mat,
  S = S,
  mu = copula.mu,
  cov_mat = copula.cov_mat
)})
time_4

time_5 = system.time({dt_5 <- shapr::prepare_data_copula_cpp(
  MC_samples_mat = MC_samples_mat,
  x_explain_mat = x_explain_mat,
  x_explain_gaussian_mat = copula.x_explain_gaussian_mat,
  x_train_mat = x_train_mat,
  S = S,
  mu = copula.mu,
  cov_mat = copula.cov_mat
)})

rbind(time_1, time_2, time_3, time_4, time_5)
all.equal(dt_1, dt_2)
all.equal(dt_2, dt_3)
all.equal(dt_3, dt_4)
all.equal(dt_4, dt_5)

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

# The C++ code with my own quantile function
time_only_cpp_sourceCpp <- system.time({
  res_only_cpp_sourceCpp <- prepare_data.copula_sourceCpp(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions]
  )
})
data.table::setorderv(res_only_cpp_sourceCpp, c("id", "id_combination"))
time_only_cpp_sourceCpp

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
  time_only_cpp_sourceCpp,
  time_only_cpp_arma,
  time_cpp_and_R
)
times

# TIMES for all coalitions (254), n_samples <- 1000, n_train <- 1000, n_test <- 20, M <- 8

#                         user.self sys.self elapsed user.child sys.child
# time_only_R                65.266    2.142  70.896          0         0
# time_only_cpp               4.622    0.393   5.212          0         0
# time_only_cpp_sourceCpp     1.823    0.423   2.279          0         0
# time_only_cpp_arma         23.874    0.604  27.801          0         0
# time_cpp_and_R              6.826    1.493   8.683          0         0

# Relative speedup of new method
times_relative <- t(sapply(seq_len(nrow(times)), function(idx) times[1, ] / times[idx, ]))
rownames(times_relative) <- paste0(rownames(times), "_rel")
times_relative

# RELATIVE TIMES for all coalitions, n_samples <- 1000, n_train <- 1000, n_test <- 20, M <- 8
#                             user.self sys.self elapsed user.child sys.child
# time_only_R_rel                1.0000   1.0000  1.0000        NaN       NaN
# time_only_cpp_rel             14.1207   5.4504 13.6025        NaN       NaN
# time_only_cpp_sourceCpp_rel   35.8014   5.0638 31.1084        NaN       NaN
# time_only_cpp_arma_rel         2.7338   3.5464  2.5501        NaN       NaN
# time_cpp_and_R_rel             9.5614   1.4347  8.1649        NaN       NaN

# Aggregate the MC sample values for each explicand and combination
res_only_R <- res_only_R[, w := NULL]
res_only_cpp <- res_only_cpp[, w := NULL]
res_only_cpp_sourceCpp <- res_only_cpp_sourceCpp[, w := NULL]
res_only_cpp_arma <- res_only_cpp_arma[, w := NULL]
res_cpp_and_R <- res_cpp_and_R[, w := NULL]
res_only_R_agr <- res_only_R[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_agr <- res_only_cpp[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_sourceCpp_agr <- res_only_cpp_sourceCpp[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_arma_agr <- res_only_cpp_arma[, lapply(.SD, mean), by = c("id", "id_combination")]
res_cpp_and_R_agr <- res_cpp_and_R[, lapply(.SD, mean), by = c("id", "id_combination")]

# Difference
res_only_R_agr - res_only_cpp_agr
res_only_R_agr - res_only_cpp_sourceCpp_agr
res_only_R_agr - res_only_cpp_arma_agr
res_only_R_agr - res_cpp_and_R_agr

# Max absolute difference
max(abs(res_only_R_agr - res_only_cpp_agr))
max(abs(res_only_R_agr - res_only_cpp_sourceCpp_agr))
max(abs(res_only_R_agr - res_only_cpp_arma_agr))
max(abs(res_only_R_agr - res_cpp_and_R_agr))

# Max absolute relative difference
max(abs(res_only_R_agr - res_only_cpp_agr) / res_only_cpp_agr)
max(abs(res_only_R_agr - res_only_cpp_sourceCpp_agr) / res_only_cpp_sourceCpp_agr)
max(abs(res_only_R_agr - res_only_cpp_arma_agr) / res_only_cpp_arma_agr)
max(abs(res_only_R_agr - res_cpp_and_R_agr) / res_cpp_and_R_agr)

# Compare gaussian_transform --------------------------------------------------------------------------------------
set.seed(123)
x_temp_rows = 10000
x_temp_cols = 20
x_temp = matrix(rnorm(x_temp_rows*x_temp_cols), x_temp_rows, x_temp_cols)

# Compare for equal values
system.time({gaussian_transform_R_res = apply(X = x_temp, MARGIN = 2, FUN = gaussian_transform_old)})
system.time({gaussian_transform_cpp_res = gaussian_transform_cpp(x_temp)})
system.time({gaussian_transform_cpp_sourceCpp_res = gaussian_transform_cpp_sourceCpp(x_temp)})
all.equal(gaussian_transform_R_res, gaussian_transform_cpp_res) # TRUE
all.equal(gaussian_transform_R_res, gaussian_transform_cpp_sourceCpp_res) # TRUE

# Compare time (generate new data each time such that the result is not stored in the cache)
rbenchmark::benchmark(R = apply(X = matrix(rnorm(x_temp_rows*x_temp_cols), x_temp_rows, x_temp_cols),
                                MARGIN = 2,
                                FUN = gaussian_transform_old),
                      cpp = gaussian_transform_cpp(matrix(rnorm(x_temp_rows*x_temp_cols), x_temp_rows, x_temp_cols)),
                      cpp_sourceCpp = gaussian_transform_cpp_sourceCpp(matrix(rnorm(x_temp_rows*x_temp_cols),
                                                                              x_temp_rows,
                                                                              x_temp_cols)),
                      replications = 100)
#            test replications elapsed relative user.self sys.self user.child sys.child
# 2           cpp          100   7.604    1.987     7.059    0.294          0         0
# 3 cpp_sourceCpp          100   3.827    1.000     3.529    0.254          0         0
# 1             R          100   6.183    1.616     4.899    0.738          0         0


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
system.time({cpp_sourceCpp = gaussian_transform_separate_cpp_sourceCpp(x_explain_temp, x_train_temp)})
all.equal(r, cpp)
all.equal(r, cpp_sourceCpp)

# Rcpp::sourceCpp("src/Copula.cpp") # C++ code is faster when I recompile it? I don't understand.
rbenchmark::benchmark(r = apply(X = rbind(x_explain_temp, x_train_temp),
                                MARGIN = 2,
                                FUN = gaussian_transform_separate_old,
                                n_y = nrow(x_explain_temp)),
                      cpp = gaussian_transform_separate_cpp(x_explain_temp, x_train_temp),
                      cpp_sourceCpp = gaussian_transform_separate_cpp_sourceCpp(x_explain_temp, x_train_temp),
                      replications = 20)
#            test replications elapsed relative user.self sys.self user.child sys.child
# 2           cpp           20  10.933    2.352    10.082    0.179          0         0
# 3 cpp_sourceCpp           20   4.648    1.000     4.389    0.100          0         0
# 1             r           20   9.787    2.106     8.409    0.797          0         0

rbenchmark::benchmark(r = apply(X = rbind(matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols),
                                          matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                                MARGIN = 2,
                                FUN = gaussian_transform_separate_old,
                                n_y = nrow(matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols))),
                      cpp = gaussian_transform_separate_cpp(matrix(rnorm(x_explain_rows*x_cols),
                                                                   x_explain_rows,
                                                                   x_cols),
                                      matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      cpp2 = .Call(`_shapr_gaussian_transform_separate_cpp`,
                                   matrix(rnorm(x_explain_rows*x_cols), x_explain_rows, x_cols),
                                   matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      cpp_cpp_sourceCpp = gaussian_transform_separate_cpp_sourceCpp(matrix(rnorm(x_explain_rows*x_cols),
                                                                   x_explain_rows,
                                                                   x_cols),
                                                            matrix(rnorm(x_train_rows*x_cols), x_train_rows, x_cols)),
                      replications = 20)

#                test replications elapsed relative user.self sys.self user.child sys.child
# 2               cpp           20  12.634    2.202    11.275    0.352       0.00      0.00
# 4 cpp_cpp_sourceCpp           20   5.737    1.000     5.287    0.182       0.00      0.00
# 3              cpp2           20  11.566    2.016    10.890    0.246       0.01      0.01
# 1                 r           20  11.937    2.081    10.232    1.027       0.00      0.00





# Simple C examples compile issues time --------------------------------------------------------------------------------
sourceCpp(
  code = '
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
Rcpp::NumericVector addVectors(const Rcpp::NumericVector& vec1, const Rcpp::NumericVector& vec2) {
  // Check if the input vectors are of the same length
  if (vec1.size() != vec2.size()) {
    Rcpp::stop("Vectors must be of the same length.");
  }

  // Create a result vector of the same length as the input vectors
  Rcpp::NumericVector result(vec1.size());

  // Perform element-wise addition
  for (int i = 0; i < vec1.size(); ++i) {
    result[i] = vec1[i] + vec2[i];
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix addMatrices(const Rcpp::NumericMatrix& mat1, const Rcpp::NumericMatrix& mat2) {
  // Check if the input matrices have the same dimensions
  if (mat1.nrow() != mat2.nrow() || mat1.ncol() != mat2.ncol()) {
    Rcpp::stop("Matrices must have the same dimensions.");
  }

  // Create a result matrix of the same dimensions as the input matrices
  Rcpp::NumericMatrix result(mat1.nrow(), mat1.ncol());

  // Perform element-wise addition
  for (int i = 0; i < mat1.nrow(); ++i) {
    for (int j = 0; j < mat1.ncol(); ++j) {
      result(i, j) = mat1(i, j) + mat2(i, j);
    }
  }

  return result;
}

// [[Rcpp::export]]
arma::mat addMatricesArmadillo(const arma::mat& mat1, const arma::mat& mat2) {
  // Check if the input matrices have the same dimensions
  if (mat1.n_rows != mat2.n_rows || mat1.n_cols != mat2.n_cols) {
    Rcpp::stop("Matrices must have the same dimensions.");
  }

  // Perform element-wise addition using Armadillo
  arma::mat result = mat1 + mat2;

  return result;
}')


# !!!!!READ!!!!!
# Copy the code above into `src/Copula.cpp` and then build the package with
devtools::load_all(".")

# Dimension of matrix
n = 1000000
m = 100

# Make matrices
mat1 = matrix(rnorm(n*m), n, m)
mat2 = matrix(rnorm(n*m), n, m)

# Time when using the compiled code using `devtools::load_all()`
shapr_vec_time = system.time({shapr_vec_res = addVectors(mat1[,1], mat2[,1])})
shapr_mat_rcpp_time = system.time({shapr_mat_rcpp_res <- addMatrices(mat1, mat2)})
shapr_mat_arma_time = system.time({shapr_mat_arma_res <- addMatricesArmadillo(mat1, mat2)})

# Then we compile with `Rcpp::compileAttributes()`
Rcpp::compileAttributes(pkgdir = ".", verbose = TRUE)
compileAttributes_vec_time = system.time({compileAttributes_vec_res = addVectors(mat1[,1], mat2[,1])})
compileAttributes_mat_rcpp_time = system.time({compileAttributes_mat_rcpp_res <- addMatrices(mat1, mat2)})
compileAttributes_mat_arma_time = system.time({compileAttributes_mat_arma_res <- addMatricesArmadillo(mat1, mat2)})

# Then we compile with `Rcpp::sourceCpp()`
# Here a shared library is built
Rcpp::sourceCpp("src/Copula.cpp", verbose = TRUE)
sourceCpp_vec_time = system.time({sourceCpp_vec_res = addVectors(mat1[,1], mat2[,1])})
sourceCpp_mat_rcpp_time = system.time({sourceCpp_mat_rcpp_res <- addMatrices(mat1, mat2)})
sourceCpp_mat_arma_time = system.time({sourceCpp_mat_arma_res <- addMatricesArmadillo(mat1, mat2)})

# Look at the times. See a drastic decrease when using sourceCpp. Half on my mac
rbind(shapr_vec_time,
      compileAttributes_vec_time,
      sourceCpp_vec_time)
rbind(shapr_mat_rcpp_time,
      compileAttributes_mat_rcpp_time,
      sourceCpp_mat_rcpp_time)
rbind(shapr_mat_arma_time,
      compileAttributes_mat_arma_time,
      sourceCpp_mat_arma_time)

# All equal
all.equal(shapr_vec_res, compileAttributes_vec_res)
all.equal(shapr_vec_res, sourceCpp_vec_res)

all.equal(shapr_mat_rcpp_res, compileAttributes_mat_rcpp_res)
all.equal(shapr_mat_rcpp_res, sourceCpp_mat_rcpp_res)

all.equal(shapr_mat_arma_res, compileAttributes_mat_arma_res)
all.equal(shapr_mat_arma_res, sourceCpp_mat_arma_res)




# Large n_samples equal results ----------------------------------------------------------------------------------------
{
  n_samples <- 1000000
  n_train <- 1000
  n_test <- 5
  M <- 4
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

# The C++ code with my own quantile function
time_only_cpp_sourceCpp <- system.time({
  res_only_cpp_sourceCpp <- prepare_data.copula_sourceCpp(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions]
  )
})
data.table::setorderv(res_only_cpp_sourceCpp, c("id", "id_combination"))
time_only_cpp_sourceCpp

# Look at the differences
# Aggregate the MC sample values for each explicand and combination
# res_only_R <- res_only_R[, w := NULL]
# res_only_cpp <- res_only_cpp[, w := NULL]
# res_only_cpp_sourceCpp <- res_only_cpp_sourceCpp[, w := NULL]
res_only_R_agr <- res_only_R[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_agr <- res_only_cpp[, lapply(.SD, mean), by = c("id", "id_combination")]
res_only_cpp_sourceCpp_agr <- res_only_cpp_sourceCpp[, lapply(.SD, mean), by = c("id", "id_combination")]

# Difference
res_only_R_agr - res_only_cpp_agr
res_only_R_agr - res_only_cpp_sourceCpp_agr

# Max absolute difference
max(abs(res_only_R_agr - res_only_cpp_agr))
max(abs(res_only_R_agr - res_only_cpp_sourceCpp_agr))

# Look at the difference in Shapley values
temp_shapley_value_func = function(dt, internal, model, predict_model) {
  compute_preds(
    dt, # Updating dt by reference
    feature_names = internal$parameters$feature_names,
    predict_model = predict_model,
    model = model,
    pred_cols = paste0("p_hat", seq_len(internal$parameters$output_size)),
    type = internal$parameters$type,
    horizon = internal$parameters$horizon,
    n_endo = internal$data$n_endo,
    explain_idx = internal$parameters$explain_idx,
    explain_lags = internal$parameters$explain_lags,
    y = internal$data$y,
    xreg = internal$data$xreg
  )
  dt_vS2 <- compute_MCint(dt, paste0("p_hat", seq_len(internal$parameters$output_size)))
  dt_vS <- rbind(t(as.matrix(c(1, rep(prediction_zero, n_test)))), dt_vS2, t(as.matrix(c(2^M, response_test))),
                 use.names = FALSE)
  colnames(dt_vS) = colnames(dt_vS2)
  compute_shapley_new(internal, dt_vS)
}

# Compute the Shapley values
res_shapley_R = temp_shapley_value_func(data.table::copy(res_only_R), internal, model, predict_model)
res_shapley_cpp = temp_shapley_value_func(data.table::copy(res_only_cpp), internal, model, predict_model)
res_shapley_cpp_sourceCpp = temp_shapley_value_func(data.table::copy(res_only_cpp_sourceCpp),
                                                    internal,
                                                    model,
                                                    predict_model)
# Look at the difference
abs(res_shapley_R - res_shapley_cpp)
abs(res_shapley_R - res_shapley_cpp_sourceCpp)
max(abs(res_shapley_R - res_shapley_cpp))
max(abs(res_shapley_R - res_shapley_cpp_sourceCpp))

# When   n_samples <- 1000000, n_train <- 1000, n_test <- 5, M <- 4
# > abs(res_shapley_R - res_shapley_cpp)
#          none         X1         X2         X3         X4
# 1: 7.2140e-11 0.00056643 0.00109848 9.5478e-05 0.00043657
# 2: 4.3903e-10 0.00179695 0.00163158 1.8549e-03 0.00202031
# 3: 9.3072e-11 0.00142949 0.00087037 1.2457e-03 0.00180482
# 4: 5.1367e-11 0.00079767 0.00099899 7.2505e-04 0.00052373
# 5: 3.8260e-10 0.00032232 0.00046644 1.1651e-03 0.00102102
# > abs(res_shapley_R - res_shapley_cpp_sourceCpp)
#          none         X1         X2         X3         X4
# 1: 3.1773e-10 0.00061369 0.00096567 0.00139486 0.00174684
# 2: 2.1354e-10 0.00164283 0.00139693 0.00051290 0.00075879
# 3: 1.2370e-10 0.00143125 0.00066145 0.00021455 0.00055524
# 4: 2.0396e-10 0.00090834 0.00091129 0.00077478 0.00077773
# 5: 1.3627e-10 0.00038308 0.00033615 0.00031426 0.00026733
# > max(abs(res_shapley_R - res_shapley_cpp))
# [1] 0.0020203
# > max(abs(res_shapley_R - res_shapley_cpp_sourceCpp))
# [1] 0.0017468
