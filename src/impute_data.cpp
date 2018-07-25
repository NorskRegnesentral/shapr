#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get imputed data
//'
//' @param ID Positive integer vector
//' @param Comb Positive integer vector
//' @inheritParams global_arguments
//'
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
NumericMatrix impute_cpp(IntegerVector ID, IntegerVector Comb, NumericMatrix Xtrain, NumericMatrix Xtest, IntegerMatrix S) {

    NumericMatrix X(ID.length(), Xtrain.ncol());

    for (int i = 0; i < X.nrow(); ++i) {

        for (int j = 0; j < X.ncol(); ++j) {

            if (S(Comb[i] - 1, j) > 0) {
                X(i, j) = Xtest(0, j);
            } else {
                X(i, j) = Xtrain(ID[i] - 1, j);
            }

        }
    }

    return X;
}
