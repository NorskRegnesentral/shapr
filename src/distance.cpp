#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param Xtrain Dataframe
//' @param Xtest Dataframe
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<double> distance_cpp(NumericMatrix Xtrain, NumericMatrix Xtest) {

    // Define variables
    int ntrain, ntest, nfeatures;
    ntrain = Xtrain.nrow();
    ntest = Xtest.nrow();
    nfeatures = Xtrain.ncol();
    arma::cube X(ntrain, ntest, nfeatures, arma::fill::zeros);

    for (int k = 0; k < nfeatures; ++k) {

        for (int j = 0; j < ntest; ++j) {

            for (int i = 0; i < ntrain; ++i) {

                X(i, j, k) = pow(Xtrain(i, k) - Xtest(j, k), 2.0);
            }

        }
    }

    return X;
}
