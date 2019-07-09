#include <RcppArmadillo.h>
using namespace Rcpp;

//' Calculate weight matrix
//'
//' @param features List. Each of the elements equals an integer
//' vector representing a valid combination of features.
//' @param m Integer. Number of features
//' @param n Integer. Number of combinations
//' @param w Numeric vector of length \code{n}, i.e. \code{w[i]} equals
//' the shapley weight of feature combination \code{i}, represented by
//' \code{features[[i]]}.
//'
//' @export
//'
//' @return Matrix of dimension n x m + 1
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::mat weight_matrix_cpp(List features, int m, int n, NumericVector w){

    // Note that Z is a n x (m + 1) matrix, where m is the number
    // of unique features. All elements in the first column is equal to 1.
    // For j > 0, Z(i, j) = 1 if and only if feature j is present in
    // the ith combination of features. In example, if Z(i, j) = 1 we know that
    // j is present in l[i].

    // Note that w represents the diagonal in W, where W is a diagoanl
    // n x n matrix.

    // Note that X.t() equals Z.t() * W, where w is the diagonal of W, which by
    // definition equals that X = W * Z. Since W is a diagonal matrix we could
    // simplify this so that X(i, j) = sum(W(i, k) * Z(k, j)) = W(i, i) * Z(i, j))
    // for k = {1, ..., n}.

    // Note that W represents a (m +1) * n matrix, i.e. W = (X.t() * Z)^-1 * X.t(),
    // where X.t() = Z.t() * W.

    // Define objects
    int nfeatures;
    IntegerVector feature_vec;
    arma::mat Z(n, m + 1, arma::fill::zeros);
    arma::mat X(n, m + 1, arma::fill::zeros);
    arma::mat W(m + 1, n, arma::fill::zeros);

    // Populate Z
    for (int i = 0; i < n; i++) {

        // Set all elements in the first column equal to 1
        Z(i, 0) = 1;

        // Extract features combinations
        feature_vec = features[i];
        nfeatures = feature_vec.length();
        if (nfeatures > 0) {
            for (int j = 0; j < nfeatures; j++)
                Z(i, feature_vec[j]) = 1;
        }
    }

    // Populate X
    for (int i = 0; i < n; i++) {

        for (int j = 0; j < Z.n_cols; j++) {

            X(i, j) = w[i] * Z(i, j);
        }
    }

    W = inv(X.t() * Z) * X.t();

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

    // Error-check
    IntegerVector features_zero = features[0];
    if (features_zero.length() > 0)
        Rcpp::stop("The first element of features should be an empty vector, i.e. integer(0)");

    for (int i = 1; i < ncomb; ++i) {

        NumericVector feature_vec = features[i];

        for (int j = 0; j < feature_vec.length(); ++j) {

            A(i, feature_vec[j] - 1) = 1.0;
        }
    }

    return A;
}

