#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param comb List
//' @param v Postive numberic vector
//' @param m Postive integer. Number of features
//' @param n Postive integer. Number of combinations
//'
//' @export
//'
//' @return Matrix of dimension n x m + 1
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Mat<double> weighted_matrix(List comb, NumericVector w, int m, int n) {

    // Define variables
    int s;
    IntegerVector ind;
    arma::Mat<double> X(n, m + 1), Xw(n, m + 1);

    // Populate matrix
    for (int i = 0; i < n; i++) {

        ind = comb[i];
        s = ind.length();
        if (s > 0) {
            for (int j = 0; j < s; j++)
                X(i, ind[j]) = 1;
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
    arma::Mat<double> W = inv(Xw * X) * Xw;

    return W;
}
