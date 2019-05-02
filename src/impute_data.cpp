#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get imputed data
//'
//' @param id Positive integer vector
//' @param Comb Positive integer vector
//' @inheritParams global_arguments
//'
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
NumericMatrix observation_impute_cpp(IntegerVector id, IntegerVector Comb, NumericMatrix xtrain, NumericMatrix xtest, IntegerMatrix s) {

    NumericMatrix x(id.length(), xtrain.ncol());

    for (int i = 0; i < x.nrow(); ++i) {

        for (int j = 0; j < x.ncol(); ++j) {

            if (s(Comb[i] - 1, j) > 0) {
                x(i, j) = xtest(0, j);
            } else {
                x(i, j) = xtrain(id[i] - 1, j);
            }

        }
    }

    return x;
}
