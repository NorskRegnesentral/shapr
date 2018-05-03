#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get imputed data
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
arma::Mat<double> impute_cpp(arma::Cube<int> I, DataFrame train, DataFrame test, List comb) {

    int nsamples, ntrain, ntest, nfeatures, ncomb, counter;
    nsamples = I.n_rows;
    ntrain = train.nrows();
    ntest = test.nrows();
    nfeatures = train.ncol();
    ncomb = I.n_slices;
    arma::mat X(nsamples * ntest * ncomb, nfeatures + 2, arma::fill::zeros);
    NumericVector impute_train;
    IntegerVector combinations;
    int ind;

    counter = 0;
    for (int i = 0; i < ntest; ++i) {

        for (int j = 0; j < ncomb; ++j) {

            for (int s = 0; s < nsamples; ++s) {

                // Add keys
                X(counter, 0) = i + 1;
                X(counter, 1) = j + 1;

                for (int k = 0; k < nfeatures; ++k) {

                    impute_train = train[k];
                    ind = arma::as_scalar(I(arma::span(s), arma::span(i), arma::span(j)));
                    X(counter, k + 2) = impute_train[ind - 1];
                }
                counter = counter + 1;
            }
        }
    }

    return X;
}
