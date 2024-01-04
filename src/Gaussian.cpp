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

   // Pre-allocate result matrix
   arma::mat ret(n_samples, n_features);

   // Create a list containing the MC samples for all coalitions and test observations
   Rcpp::List result_list;

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < S.n_rows; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       ret.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1); // can using .fill() speed this up?
       ret.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
       result_list.push_back(ret);
     }
   }

   return result_list;
 }

 // [[Rcpp::export]]
 Rcpp::List prepare_data_gaussian_cpp_with_wrap(arma::mat MC_samples_mat,
                                                arma::mat x_explain_mat,
                                                arma::mat S,
                                                arma::vec mu,
                                                arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;

   // Pre-allocate result matrix
   arma::mat ret(n_samples, n_features);

   // Create a list containing the MC samples for all coalitions and test observations
   Rcpp::List result_list;

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < S.n_rows; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       ret.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1); // can using .fill() speed this up?
       ret.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
       result_list.push_back(Rcpp::wrap(ret));
     }
   }

   return result_list;
 }

 // [[Rcpp::export]]
 Rcpp::List prepare_data_gaussian_cpp_v2(arma::mat MC_samples_mat,
                                         arma::mat x_explain_mat,
                                         arma::mat S,
                                         arma::vec mu,
                                         arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;

   // Create a list containing the MC samples for all coalitions and test observations
   Rcpp::List result_list;

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


     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = trans(MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S));

     // Loop over the different test observations and Combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       arma::mat ret(n_samples, n_features);
       ret.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);
       ret.cols(Sbar_now_idx) = trans(MC_samples_mat_now + repmat(x_Sbar_mean.col(idx_now), 1, n_samples));
       result_list.push_back(ret);
     }
   }

   return result_list;
 }

 // [[Rcpp::export]]
 arma::mat prepare_data_gaussian_cpp_fix_large_mat(arma::mat MC_samples_mat,
                                                   arma::mat x_explain_mat,
                                                   arma::mat S,
                                                   arma::vec mu,
                                                   arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;
   int n_coalitions = S.n_rows;

   // Pre-allocate result matrix
   arma::mat return_mat(n_coalitions*n_explain*n_samples, n_features);

   // Create a list containing the MC samples for all coalitions and test observations
   std::list<arma::mat> result_list;
   // Rcpp::List result_list;

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       // Maybe faster to create vector 0:(n_samples - 1) and then just add n_samples in each loop.
       arma::uvec row_indices_now = arma::linspace<arma::uvec>(S_ind*n_explain*n_samples + idx_now*n_samples,
                                                               S_ind*n_explain*n_samples + idx_now*n_samples + n_samples - 1,
                                                               n_samples);

       return_mat.submat(row_indices_now, S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);
       return_mat.submat(row_indices_now, Sbar_now_idx) =
         MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
     }
   }

   return return_mat;
 }

 // Diff in v2 is where we do the transpose
 // [[Rcpp::export]]
 arma::mat prepare_data_gaussian_cpp_fix_large_mat_v2(arma::mat MC_samples_mat,
                                                      arma::mat x_explain_mat,
                                                      arma::mat S,
                                                      arma::vec mu,
                                                      arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;
   int n_coalitions = S.n_rows;

   // Pre-allocate result matrix
   arma::mat return_mat(n_coalitions*n_explain*n_samples, n_features);

   // Create a list containing the MC samples for all coalitions and test observations
   std::list<arma::mat> result_list;
   // Rcpp::List result_list;

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = trans(MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S));

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       // Maybe faster to create vector 0:(n_samples - 1) and then just add n_samples in each loop.
       arma::uvec row_indices_now = arma::linspace<arma::uvec>(S_ind*n_explain*n_samples + idx_now*n_samples,
                                                               S_ind*n_explain*n_samples + idx_now*n_samples + n_samples - 1,
                                                               n_samples);

       return_mat.submat(row_indices_now, S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);
       return_mat.submat(row_indices_now, Sbar_now_idx) =
         trans(MC_samples_mat_now + repmat(x_Sbar_mean.col(idx_now), 1, n_samples));
     }
   }

   return return_mat;
 }

 // [[Rcpp::export]]
 arma::cube prepare_data_gaussian_cpp_fix_cube(arma::mat MC_samples_mat,
                                               arma::mat x_explain_mat,
                                               arma::mat S,
                                               arma::vec mu,
                                               arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;
   int n_coalitions = S.n_rows;

   // Pre-allocate result matrix
   arma::mat aux_mat(n_samples, n_features);
   arma::cube result_cube(n_samples, n_features, n_explain*n_coalitions);

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1); // can using .fill() speed this up?
       aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
       result_cube.slice(S_ind*n_explain + idx_now) = aux_mat;
     }
   }

   return result_cube;
 }

 // [[Rcpp::export]]
 arma::cube prepare_data_gaussian_cpp_fix_cube_v2(arma::mat MC_samples_mat,
                                                  arma::mat x_explain_mat,
                                                  arma::mat S,
                                                  arma::vec mu,
                                                  arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;
   int n_coalitions = S.n_rows;

   // Pre-allocate result matrix
   arma::mat aux_mat(n_samples, n_features);
   arma::cube result_cube(n_samples, n_explain*n_coalitions, n_features);

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < n_coalitions; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1); // can using .fill() speed this up?
       aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
       result_cube.col(S_ind*n_explain + idx_now) = aux_mat;
     }
   }

   return result_cube;
 }

 // [[Rcpp::export]]
 Rcpp::List prepare_data_gaussian_cpp_fix_list_of_lists_of_matrices(arma::mat MC_samples_mat,
                                                                    arma::mat x_explain_mat,
                                                                    arma::mat S,
                                                                    arma::vec mu,
                                                                    arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;

   // Pre-allocate result matrix
   arma::mat aux_mat(n_samples, n_features);

   // Create a list containing lists that contian the MC samples for all coalitions and test observations in each matrix
   Rcpp::List result_list(S.n_rows);

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < S.n_rows; S_ind++) {

     Rcpp::List result_list_now(n_explain);

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1); // can using .fill() speed this up?
       aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
       result_list_now[idx_now] = aux_mat;
     }
     result_list[S_ind] = result_list_now;
   }

   return result_list;
 }

 // [[Rcpp::export]]
 std::list<arma::mat> prepare_data_gaussian_cpp_fix_std_list(arma::mat MC_samples_mat,
                                                             arma::mat x_explain_mat,
                                                             arma::mat S,
                                                             arma::vec mu,
                                                             arma::mat cov_mat) {
   int n_explain = x_explain_mat.n_rows;
   int n_samples = MC_samples_mat.n_rows;
   int n_features = MC_samples_mat.n_cols;

   // Pre-allocate result matrix
   arma::mat aux_mat(n_samples, n_features);

   // Create a list containing the MC samples for all coalitions and test observations
   std::list<arma::mat> result_list;

   // Iterate over the coalitions
   for (int S_ind = 0; S_ind < S.n_rows; S_ind++) {

     // TODO: REMOVE IN FINAL VERSION Small printout
     Rcpp::Rcout << S_ind + 1 << ",";

     // Get current coalition S and the indices of the features in coalition S and mask Sbar
     arma::mat S_now = S.row(S_ind);
     arma::uvec S_now_idx = arma::find(S_now > 0.5); // må finnes en bedre løsning her
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

     // Ensure that the conditional covariance matrix is symmetric
     if (!cond_cov_mat_Sbar_given_S.is_symmetric()) {
       cond_cov_mat_Sbar_given_S = arma::symmatl(cond_cov_mat_Sbar_given_S);
     }

     // Compute the conditional mean of Xsbar given Xs = Xs_star
     arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star.each_row() - mu_S.t()).t(); // Can we speed it up by reducing the number of transposes?
     x_Sbar_mean.each_col() += mu_Sbar;

     // Transform the samples to be from N(O, Sigma_Sbar|S)
     arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

     // Loop over the different test observations and combine the generated values with the values we conditioned on
     for (int idx_now = 0; idx_now < n_explain; idx_now++) {
       aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1); // can using .fill() speed this up?
       aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
       result_list.push_back(aux_mat);
     }
   }

   return result_list;
 }
