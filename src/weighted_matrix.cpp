#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param n Positive integer. Number of combinations
//' @inheritParams global_arguments
//'
//' @export
//'
//' @return Matrix of dimension n x m + 1
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::mat weight_matrix_cpp(List features, int m, int n, NumericVector w){

    // Define variables
    int nfeatures;
    IntegerVector feature_vec;
    arma::mat X(n, m + 1, arma::fill::zeros), Xw(n, m + 1, arma::fill::zeros);
    arma::mat W(m + 1, n, arma::fill::zeros);

    // Populate matrix
    for (int i = 0; i < n; i++) {

        feature_vec = features[i];
        nfeatures = feature_vec.length();
        if (nfeatures > 0) {
            for (int j = 0; j < nfeatures; j++)
              X(i, feature_vec[j]) = 1;
        }
    }

    // Set first column to 1
    for (int i = 0; i < n; i++) {
      X(i, 0) = 1;
    }

    // Multiple weights
    for (int i = 0; i < n; i++) {

        for (int j = 0; j < X.n_cols; j++) {

            Xw(i, j) = w[i] * X(i, j);
        }
    }

    Xw = Xw.t();
    W = inv(Xw * X) * Xw;
    return W;
}

//' Get feature matrix
//'
//' @param features List
//' @param nfeatures Positive integer. Total number of features
//'
//' @export
//'
//' @return Matrix
//' @author Nikolai Sellereite
// [[Rcpp::export]]
NumericMatrix feature_matrix_cpp(List features, int nfeatures) {

    // Define variables
    int ncomb;
    ncomb = features.length();
    NumericMatrix A(ncomb, nfeatures);

    for (int i = 1; i < ncomb; ++i) {

        NumericVector feature_vec = features[i];

        for (int j = 0; j < feature_vec.length(); ++j) {

            A(i, feature_vec[j] - 1) = 1.0;
        }
    }

    return A;
}

