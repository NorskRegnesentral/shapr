#include <RcppArmadillo.h>
#include <iostream>
using namespace Rcpp;


//' Generate Gaussian MC samples
//'
//' @param MC_samples_mat matrix. Matrix of dimension `n_samples` times `n_features` containing samples from the
//' univariate standard normal.
//' @param x_explain_mat matrix. Matrix of dimension `n_explain` times `n_features` containing the observations
//' to explain.
//' @param S matrix. Matrix of dimension `n_combinations` times `n_features` containing binary representations of
//' the used coalitions.
//' @param mu vector. Vector of length `n_features` containing the mean of each feature.
//' @param cov_mat mat. Matrix of dimension `n_features` times `n_features` containing the pariwise covariance between
//' all features.
//'
//' @export
//' @keywords internal
//'
//' @return List of length `n_combinations`*`n_samples`, where each entry is a matrix of dimension `n_samples` times
//' `n_features` containing the conditional MC samples for each coalition and explicand.
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
Rcpp::List prepare_data_gaussian_cpp(arma::mat MC_samples_mat,
                                     arma::mat x_explain_mat,
                                     arma::mat S,
                                     arma::vec mu,
                                     arma::mat cov_mat) {
  int n_explain = x_explain_mat.n_rows;
  int n_samples = MC_samples_mat.n_rows;
  int n_features = MC_samples_mat.n_cols;

  // Create a list containing the MC samples for all coalitions and test observations
  Rcpp::List resultList;

  // Iterate over the coalitions
  for (int S_ind = 0; S_ind < S.n_rows; S_ind++) {

    // TODO: REMOVE IN FINAL VERSION Small printout
    Rcpp::Rcout << S_ind + 1 << ",";

    // Get current coalition S and the indices of the features in coalition S and mask Sbar
    arma::mat S_now = S.row(S_ind);
    arma::uvec S_now_idx = arma::find(S_now > 0.5);
    arma::uvec Sbar_now_idx = arma::find(S_now < 0.5);

    // Extract the features we condition on
    arma::mat x_S_star = x_explain_mat.cols(S_now_idx);

    // Extract the mean values for the features in the two sets
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

    // Ensure that the conditional covariance matrix is symmetric and positive definite(?)
    if (!cond_cov_mat_Sbar_given_S.is_sympd()) {
      cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
    }

    // Compute the conditional mean of Xsbar given Xs = Xs_star
    arma::mat x_Sbar_mean = (cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t()); // Can we speed it up by reducing the number of transposes?
    x_Sbar_mean.each_col() += mu_Sbar;

    // Transform the samples to be from N(O, Sigma_Sbar|S)
    arma::mat MC_samples_mat_now = trans(MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S));

    // Loop over the different test observations and Combine the generated values with the values we conditioned on
    for (int idx_now = 0; idx_now < n_explain; idx_now++) {
      arma::mat ret(n_samples, n_features, arma::fill::zeros);
      ret.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);
      ret.cols(Sbar_now_idx) = trans(MC_samples_mat_now + repmat(x_Sbar_mean.col(idx_now), 1, n_samples));
      resultList.push_back(ret);
    }
  }

  return resultList;
}
