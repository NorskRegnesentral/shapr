#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param w Postive numberic vector
//' @param n Postive integer. Number of combinations
//' @inheritParams global_arguments
//'
//' @export
//'
//' @return Matrix of dimension n x m + 1
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::mat weighted_matrix(List features, NumericVector w, int m, int n) {

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
