#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param X Three dimnesional array
//'
//' @export
//'
//' @return List
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<int> sample_cpp(arma::Cube<double> X, int nSamples, int ncomb) {

    // Setup
    int ntrain, ntest;
    ntrain = X.n_rows;
    ntest = X.n_cols;
    arma::Cube<int> Y(nSamples, ntest, ncomb);
    std::default_random_engine generator;
    NumericVector weights(ntrain);

    // Loop through all combinations
    for (int k = 0; k < ncomb; ++k) {

        // Loop through test data
        for (int j = 0; j < ntest; ++j) {

            // Get weights
            for (int i = 0; i < ntrain; ++i) {
                weights[i] = arma::as_scalar(X(arma::span(i), arma::span(j), arma::span(k)));
            }

            // Define sampler
            std::discrete_distribution<int> distribution(weights.begin(), weights.end());

            for (int i = 0; i < nSamples; ++i) {
                int number = distribution(generator);
                Y(i, j, k) = number + 1;
            }
        }
    }

    return Y;
}
