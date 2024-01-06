#include <RcppArmadillo.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]

//' Transforms new data to a standardized normal distribution
//'
//' @details The function uses `arma::quantile(...)` which corresponds to R's `stats::quantile(..., type = 5)`.
//'
//' @param z arma::mat. The data are the Gaussian Monte Carlos samples to transform.
//' @param x arma::mat. The data with the original transformation. Used to conduct the transformation of `z`.
//'
//' @return arma::mat of same dimension as `z`
//'
//' @keywords internal
//' @author Lars Henry Berge Olsen
// [[Rcpp::export]]
arma::mat inv_gaussian_transform_cpp(arma::mat z, arma::mat x) {
  int n_features = z.n_cols;
  int n_samples = z.n_rows;
  arma::mat u = arma::normcdf(z);
  arma::mat z_new(n_samples, n_features);
  for (int feature_idx = 0; feature_idx < n_features; feature_idx++) {
    z_new.col(feature_idx) = arma::quantile(x.col(feature_idx), u.col(feature_idx));
  }
  return z_new;
}



// [[Rcpp::export]]
arma::cube prepare_data_copula_cpp(arma::mat MC_samples_mat,
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

    // Extract the features we condition on
    arma::mat x_S_star = x_explain_mat.cols(S_now_idx);
    arma::mat x_S_star_gaussian = x_explain_gaussian_mat.cols(S_now_idx);

    // // Does that we do not conditioning on
    // arma::mat x_Sbar_star = x_train_mat.cols(Sbar_now_idx);

    // Extract the mean values for the features in the two sets
    arma::vec mu_S = mu.elem(S_now_idx);
    arma::vec mu_Sbar = mu.elem(Sbar_now_idx);

    std::cout << mu_S << std::endl;

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
    arma::mat x_Sbar_mean = cov_mat_SbarS_cov_mat_SS_inv * (x_S_star_gaussian.each_row() - mu_S.t()).t();
    x_Sbar_mean.each_col() += mu_Sbar;

    // Transform the samples to be from N(O, Sigma_Sbar|S)
    arma::mat MC_samples_mat_now = MC_samples_mat.cols(Sbar_now_idx) * arma::chol(cond_cov_mat_Sbar_given_S);

    // Loop over the different test observations and combine the generated values with the values we conditioned on
    for (int idx_now = 0; idx_now < n_explain; idx_now++) {

      arma::mat MC_samples_mat_now_now = MC_samples_mat_now + repmat(trans(x_Sbar_mean.col(idx_now)), n_samples, 1);
      arma::mat MC_samples_mat_now_now_trans =
        inv_gaussian_transform_cpp(MC_samples_mat_now_now, x_train_mat.cols(Sbar_now_idx));

      aux_mat.cols(S_now_idx) = repmat(x_S_star.row(idx_now), n_samples, 1);
      aux_mat.cols(Sbar_now_idx) = MC_samples_mat_now_now_trans;

      result_cube.col(S_ind*n_explain + idx_now) = aux_mat;
    }
  }

  return result_cube;
}




//' Transforms new data to a standardized normal distribution
 //'
 //' @param zx Numeric vector. The first `n_samples` items are the Gaussian data, and the last part is
 //' the data with the original transformation.
 //' @param n_samples Positive integer. Number of elements of `zx` that belongs to new data.
 //'
 //' @return Numeric matrix of length `n_samples`
 //'
 //' @keywords internal
 //'
 //' @author Lars Henry Berge Olsen
 // [[Rcpp::export]]
 Rcpp::NumericVector inv_gaussian_transform_cpp_Rcpp(const Rcpp::NumericVector& zx, const int n_samples) {

   // Extract z and x
   Rcpp::NumericVector z = zx[Rcpp::Range(0, n_samples - 1)];
   Rcpp::NumericVector x = zx[Rcpp::Range(n_samples, zx.size() - 1)];

   // Calculate u
   Rcpp::NumericVector u = Rcpp::pnorm(z);

   // Calculate x_new using Armadillo's quantile function
   arma::vec x_arma = Rcpp::as<arma::vec>(x);
   arma::vec u_arma = Rcpp::as<arma::vec>(u);

   arma::vec x_new_arma = arma::quantile(x_arma, u_arma);

   // Convert back to Rcpp::NumericMatrix
   Rcpp::NumericVector x_new = Rcpp::wrap(x_new_arma);

   return x_new;
 }


// [[Rcpp::export]]
arma::mat inv_gaussian_transform_cpp_mat(Rcpp::NumericMatrix zx, const int n_samples) {

  int n_features = zx.ncol();

  // Extract z and x
  Rcpp::NumericMatrix z = zx(Rcpp::Range(0, n_samples - 1), Rcpp::_ );
  Rcpp::NumericMatrix x = zx(Rcpp::Range(n_samples, zx.nrow() - 1), Rcpp::_ );

  // Rcpp::NumericMatrix u = Rcpp::pnorm(z);

  // Convert Rcpp::NumericMatrix to arma::mat
  arma::mat z_arma = Rcpp::as<arma::mat>(z);
  arma::mat x_arma = Rcpp::as<arma::mat>(x);

  // Calculate u
  arma::mat u_arma = arma::normcdf(z_arma);

  // Calculate x_new using Armadillo's quantile function
  arma::mat x_new_arma(n_samples, n_features);
  for (int feature_idx = 0; feature_idx < n_features; feature_idx++) {
    x_new_arma.col(feature_idx) = arma::quantile(x_arma.col(feature_idx), u_arma.col(feature_idx));
  }

  // // Convert back to Rcpp::NumericVector
  // Rcpp::NumericMatrix x_new = Rcpp::wrap(x_new_arma);

  return x_new_arma;
}

// [[Rcpp::export]]
arma::mat inv_gaussian_transform_cpp_armamat(arma::mat zx, const int n_samples) {

  int n_features = zx.n_cols;

  // WHAT IS THE POINT TO FIRST ADD THEM TOGETHER AND THEN SPLIT THEM?
  // Extract z and x
  arma::mat z = zx.rows(0, n_samples - 1);
  arma::mat x = zx.rows(n_samples, zx.n_rows - 1);

  // Calculate u
  arma::mat u = arma::normcdf(z);

  // Calculate x_new using Armadillo's quantile function
  arma::mat x_new(n_samples, n_features);
  for (int feature_idx = 0; feature_idx < n_features; feature_idx++) {
    x_new.col(feature_idx) = arma::quantile(x.col(feature_idx), u.col(feature_idx));
  }

  // // Convert back to Rcpp::NumericVector
  // Rcpp::NumericMatrix x_new = Rcpp::wrap(x_new_arma);

  return x_new;
}


