#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Generate Gaussian MC samples
//'
//' @param MC_samples_mat arma::mat.
//' Matrix of dimension (`n_MC_samples`, `n_features`) containing samples from the univariate standard normal.
//' @param x_explain_mat arma::mat.
//' Matrix of dimension (`n_explain`, `n_features`) containing the observations to explain.
//' @param S arma::mat.
//' Matrix of dimension (`n_coalitions`, `n_features`) containing binary representations of the used coalitions.
//' S cannot contain the empty or grand coalition, i.e., a row containing only zeros or ones.
//' This is not a problem internally in shapr as the empty and grand coalitions are treated differently.
//' @param mu arma::vec.
//' Vector of length `n_features` containing the mean of each feature.
//' @param cov_mat arma::mat.
//' Matrix of dimension (`n_features`, `n_features`) containing the covariance matrix of the features.
//'
//' @return An arma::cube/3D array of dimension (`n_MC_samples`, `n_explain` * `n_coalitions`, `n_features`), where
//' the columns (_,j,_) are matrices of dimension (`n_MC_samples`, `n_features`) containing the conditional Gaussian
//' MC samples for each explicand and coalition.
//'
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::cube prepare_data_gaussian_cpp(const arma::mat& MC_samples_mat,
                                     const arma::mat& x_explain_mat,
                                     const arma::mat& S,
                                     const arma::vec& mu,
                                     const arma::mat& cov_mat) {

  int n_explain = x_explain_mat.n_rows;
  int n_MC_samples = MC_samples_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;
  int n_coalitions = S.n_rows;

  // Initialize auxiliary matrix and result cube
  arma::mat aux_mat(n_MC_samples, n_features);
  arma::cube result_cube(n_MC_samples, n_explain * n_coalitions, n_features);

  // Iterate over the coalitions
  for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

    // Get current coalition S and the indices of the features in coalition S and mask Sbar
    arma::mat S_now = S.row(S_ind);
    arma::uvec S_now_idx = arma::find(S_now > 0.5);
    arma::uvec Sbar_now_idx = arma::find(S_now < 0.5);

    // Extract the features we condition on
    arma::mat x_S_star = x_explain_mat.cols(S_now_idx);

    // Extract the mean values of the features in the two sets
    arma::vec mu_S = mu.elem(S_now_idx);
    arma::vec mu_Sbar = mu.elem(Sbar_now_idx);

    // Extract the relevant parts of the covariance matrix
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

    // Compute the conditional mean of Xsbar given Xs = Xs_star
    arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t();
    x_Sbar_mean.each_col() += mu_Sbar;

    // Transform the samples to be from N(O, Sigma_{Sbar|S})
    arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

    // Loop over the different explicands and combine the generated values with the values we conditioned on
    for (int idx_now = 0; idx_now < n_explain; idx_now++) {
      aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_MC_samples, 1);
      aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_MC_samples, 1);
      result_cube.col(S_ind * n_explain + idx_now) = aux_mat;
    }
  }

  return result_cube;
}

//' Generate Gaussian MC samples for the causal setup with a single MC sample for each explicand
//'
//'
//' @inherit prepare_data_gaussian_cpp
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::mat prepare_data_gaussian_cpp_caus(const arma::mat& MC_samples_mat,
                                         const arma::mat& x_explain_mat,
                                         const arma::mat& S,
                                         const arma::vec& mu,
                                         const arma::mat& cov_mat) {

  int n_explain = x_explain_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;
  int n_coalitions = S.n_rows;

  // Initialize the result matrix
  arma::mat result_mat(n_explain * n_coalitions, n_features);

  // Iterate over the coalitions
  for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

    // Get the row_indices in the result_mat for the current coalition
    arma::uvec row_vec = arma::linspace<arma::uvec>(n_explain * S_ind, n_explain * (S_ind + 1) - 1, n_explain);

    // Get current coalition S and the indices of the features in coalition S and mask Sbar
    arma::mat S_now = S.row(S_ind);
    arma::uvec S_now_idx = arma::find(S_now > 0.5);
    arma::uvec Sbar_now_idx = arma::find(S_now < 0.5);

    // Extract the features we condition on
    arma::mat x_S_star = x_explain_mat.cols(S_now_idx);

    // Extract the mean values of the features in the two sets
    arma::vec mu_S = mu.elem(S_now_idx);
    arma::vec mu_Sbar = mu.elem(Sbar_now_idx);

    // Extract the relevant parts of the covariance matrix
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

    // Compute the conditional mean of Xsbar given Xs = Xs_star
    arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t();
    x_Sbar_mean.each_col() += mu_Sbar;

    // Transform the samples to be from N(O, Sigma_{Sbar|S})
    arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

    // Combine the generated values with the values we conditioned on to generate the final MC samples and save them
    result_mat.submat(row_vec, S_now_idx) = x_S_star;
    result_mat.submat(row_vec, Sbar_now_idx) = MC_samples_mat_now + trans(x_Sbar_mean);
  }

  return result_mat;
}
