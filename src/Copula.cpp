#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Compute the quantiles using quantile type seven
//'
//' @param x arma::vec.
//' Numeric vector whose sample quantiles are wanted.
//' @param probs arma::vec.
//' Numeric vector of probabilities with values between zero and one.
//'
//' @details Using quantile type number seven from stats::quantile in R.
//'
//' @return A vector of length `length(probs)` with the quantiles is returned.
//'
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::vec quantile_type7_cpp(const arma::vec& x, const arma::vec& probs) {
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

//' Transforms new data to a standardized normal distribution
//'
//' @param z arma::mat.
//' The data are the Gaussian Monte Carlos samples to transform.
//' @param x arma::mat.
//' The data with the original transformation. Used to conduct the transformation of `z`.
//'
//' @return arma::mat of the same dimension as `z`
//'
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::mat inv_gaussian_transform_cpp(const arma::mat& z, const arma::mat& x) {
  int n_features = z.n_cols;
  int n_MC_samples = z.n_rows;
  arma::mat z_new(n_MC_samples, n_features);
  arma::mat u = arma::normcdf(z);
  for (int feature_idx = 0; feature_idx < n_features; feature_idx++) {
    z_new.col(feature_idx) = quantile_type7_cpp(x.col(feature_idx), u.col(feature_idx));
  }
  return z_new;
}

//' Generate (Gaussian) Copula MC samples
//'
//' @param x_explain_gaussian_mat arma::mat.
//' Matrix of dimension (`n_explain`, `n_features`) containing the observations to explain after being transformed
//' using the Gaussian transform, i.e., the samples have been transformed to a standardized normal distribution.
//' @param x_train_mat arma::mat.
//' Matrix of dimension (`n_train`, `n_features`) containing the training observations.
//' @param mu arma::vec.
//' Vector of length `n_features` containing the mean of each feature after being transformed using the Gaussian
//' transform, i.e., the samples have been transformed to a standardized normal distribution.
//' @param cov_mat arma::mat.
//' Matrix of dimension (`n_features`, `n_features`) containing the pairwise covariance between all pairs of features
//' after being transformed using the Gaussian transform, i.e., the samples have been transformed to a standardized
//' normal distribution.
//'
//' @return An arma::cube/3D array of dimension (`n_MC_samples`, `n_explain` * `n_coalitions`, `n_features`), where
//' the columns (_,j,_) are matrices of dimension (`n_MC_samples`, `n_features`) containing the conditional Gaussian
//' copula MC samples for each explicand and coalition on the original scale.
//'
//' @inheritParams prepare_data_gaussian_cpp
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::cube prepare_data_copula_cpp(const arma::mat& MC_samples_mat,
                                   const arma::mat& x_explain_mat,
                                   const arma::mat& x_explain_gaussian_mat,
                                   const arma::mat& x_train_mat,
                                   const arma::mat& S,
                                   const arma::vec& mu,
                                   const arma::mat& cov_mat) {

  int n_explain = x_explain_mat.n_rows;
  int n_MC_samples = MC_samples_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;
  int n_coalitions = S.n_rows;

  // Initialize auxiliary matrix and result cube
  arma::mat aux_mat(n_MC_samples, n_features);
  arma::cube result_cube(n_MC_samples, n_explain*n_coalitions, n_features);

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
        MC_samples_mat_now + repmat(trans(x_Sbar_gaussian_mean.col(idx_now)), n_MC_samples, 1);

      // Transform the MC to the original scale using the inverse Gaussian transform
      arma::mat MC_samples_mat_now_now_trans =
        inv_gaussian_transform_cpp(MC_samples_mat_now_now, x_train_mat.cols(Sbar_now_idx));

      // Insert the generate Gaussian copula MC samples and the feature values we condition on into an auxiliary matrix
      aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now_now_trans;
      aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_MC_samples, 1);

      // Insert the auxiliary matrix into the result cube
      result_cube.col(S_ind*n_explain + idx_now) = aux_mat;
    }
  }

  return result_cube;
}

//' Generate (Gaussian) Copula MC samples for the causal setup with a single MC sample for each explicand
//'
//' @inherit prepare_data_copula_cpp
//'
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::mat prepare_data_copula_cpp_caus(const arma::mat& MC_samples_mat,
                                       const arma::mat& x_explain_mat,
                                       const arma::mat& x_explain_gaussian_mat,
                                       const arma::mat& x_train_mat,
                                       const arma::mat& S,
                                       const arma::vec& mu,
                                       const arma::mat& cov_mat) {

  int n_explain = x_explain_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;
  int n_coalitions = S.n_rows;

  // Initialize auxiliary matrix and result cube
  arma::mat result_mat(n_explain * n_coalitions, n_features);

  // Iterate over the coalitions
  for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

    // Get the row_indices in the result_mat for the current coalition
    arma::uvec row_vec = arma::linspace<arma::uvec>(n_explain * S_ind, n_explain * (S_ind + 1) - 1, n_explain);

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

    // Transform the MC samples to be from N(mu_{Sbar|S}, Sigma_{Sbar|S}) for one coalition
    arma::mat MC_samples_mat_now_now = MC_samples_mat_now + trans(x_Sbar_gaussian_mean);

    // Transform the MC to the original scale using the inverse Gaussian transform
    arma::mat MC_samples_mat_now_now_trans =
      inv_gaussian_transform_cpp(MC_samples_mat_now_now, x_train_mat.cols(Sbar_now_idx));

    // Combine the generated values with the values we conditioned on to generate the final MC samples and save them
    result_mat.submat(row_vec, S_now_idx) = x_S_star;
    result_mat.submat(row_vec, Sbar_now_idx) = MC_samples_mat_now_now_trans;
  }

  return result_mat;
}
