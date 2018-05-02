#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param comb List
//' @param train Dataframe
//' @param test Dataframe
//' @param ncomb Positive integer
//' @param sigma Positive numeric
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<double> distance_cpp(List comb, DataFrame train, DataFrame test, int ncomb, double sigma) {

    // Define variables
    int ntrain, ntest, s;
    IntegerVector ind;
    ntrain = train.nrow();
    ntest = test.nrow();
    arma::Cube<double> X(ntrain, ntest, ncomb);

    for (int k = 0; k < ncomb; ++k) {

        // Get index of features
        ind = comb[k];
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
                NumericVector n_test = test[idx];
                NumericVector n_train = train[idx];

                // Loop through features
                NumericVector dist = n_train-n_test[j];
                dist = dist * dist;
                d(_, n) = dist;
            }

            NumericVector x;
            x = rowSums(d);
            x = sqrt(exp((-0.5 * x) / sigma*sigma));

            for (int xx = 0; xx < x.length(); ++xx) {
                X(xx, j, k) = x[xx];
            }
        }
    }

    return X;
}
