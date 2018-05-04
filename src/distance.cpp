#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param features List
//' @param Xtrain Dataframe
//' @param Xtest Dataframe
//' @param ncomb Positive integer
//' @param sigma Positive numeric
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<double> distance_cpp(List features, DataFrame Xtrain, DataFrame Xtest, int ncomb, double sigma) {

    // Define variables
    int ntrain, ntest, s;
    IntegerVector ind;
    ntrain = Xtrain.nrow();
    ntest = Xtest.nrow();
    arma::cube X(ntrain, ntest, ncomb, arma::fill::zeros);
    NumericVector n_test;
    NumericVector n_train;

    for (int k = 0; k < ncomb; ++k) {

        // Get index of features
        ind = features[k];
        s = ind.length();

        if (s == 0) {
            continue;
        }

        // Loop through test data
        for (int j = 0; j < ntest; ++j) {

            NumericMatrix d(ntrain, s);

            for (int n = 0; n < s; ++n) {

                // Get vectors for test and train datas
                int idx = ind[n] - 1;
                n_test = Xtest[idx];
                n_train = Xtrain[idx];

                // Loop through features
                NumericVector dist = n_train - n_test[j];
                dist = dist * dist;
                d(_, n) = dist;
            }

            NumericVector x;
            x = rowSums(d);
            x = sqrt(exp((-0.5 * x) / sigma*sigma));

            if (max(x) < 0.00001) {
                std::fill(x.begin(), x.end(), 1 / ntrain);
            }

            for (int xx = 0; xx < x.length(); ++xx) {
                X(xx, j, k) = x[xx];
            }
        }
    }

    return X;
}

//' Get distance
//'
//' @param Xtrain Dataframe
//' @param Xtest Dataframe
//' @param m Positive integer
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<double> distance_cpp2(NumericMatrix Xtrain, NumericMatrix Xtest, int m) {

    // Define variables
    int ntrain, ntest;
    ntrain = Xtrain.nrow();
    ntest = Xtest.nrow();
    arma::cube X(ntrain, ntest, m, arma::fill::zeros);

    for (int k = 0; k < m; ++k) {

        for (int j = 0; j < ntest; ++j) {

            for (int i = 0; i < ntrain; ++i) {

                X(i, j, k) = pow(Xtrain(i, k) - Xtest(j, k), 2.0);
            }

        }
    }

    return X;
}
