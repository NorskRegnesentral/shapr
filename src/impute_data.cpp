#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get imputed data
//'
//' @param index_xtrain Positive integer. Represents a sequence of row indices from \code{xtrain},
//' i.e. \code{min(index_xtrain) >= 1} and \code{max(index_xtrain) <= nrow(xtrain)}.
//'
//' @param index_s Positive integer. Represents a sequence of row indices from \code{S},
//' i.e. \code{min(index_s) >= 1} and \code{max(index_s) <= nrow(S)}.
//'
//' @param xtrain Numeric matrix.
//'
//' @param xtest Numeric matrix. Represents a single test observation.
//'
//' @param S Integer matrix of dimension \code{n_combinations x m}, where \code{n_combinations} equals
//' the total number of sampled/non-sampled feature combinations and \code{m} equals
//' the total number of unique features. Note that \code{m = ncol(xtrain)}. See details
//' for more information.
//'
//' @details \code{S(i, j) = 1} if and only if feature \code{j} is present in feature
//' combination \code{i}, otherwise \code{S(i, j) = 0}. I.e. if \code{m = 3}, there
//' are \code{2^3 = 8} unique ways to combine the features. In this case \code{dim(S) = c(8, 3)}.
//' Let's call the features \code{x1, x2, x3} and take a closer look at the combination
//' represented by \code{s = c(x1, x2)}. If this combination is represented by the second row,
//' the following is true: \code{S[2, 1:3] = c(1, 1, 0)}.
//'
//' The returned object, \code{X}, is a numeric matrix where
//' \code{dim(X) = c(length(index_xtrain), ncol(xtrain))}. If feature \code{j} is present in
//' the k-th observation, that is \code{S[index_[k], j] == 1}, \code{X[k, j] = xtest[1, j]}.
//' Otherwise \code{X[k, j] = xtrain[index_xtrain[k], j]}.
//'
//' @export
//'
//' @return Numeric matrix
//'
//' @author Nikolai Sellereite
// [[Rcpp::export]]
NumericMatrix observation_impute_cpp(IntegerVector index_xtrain,
                                     IntegerVector index_s,
                                     NumericMatrix xtrain,
                                     NumericMatrix xtest,
                                     IntegerMatrix S) {


    // Error-checks
    if (index_xtrain.length() != index_s.length())
        Rcpp::stop("The length of index_train and index_s should be equal.");

    if (xtrain.ncol() != xtest.ncol())
        Rcpp::stop("Number of columns in xtrain and xtest should be equal.");

    NumericMatrix X(index_xtrain.length(), xtrain.ncol());

    for (int i = 0; i < X.nrow(); ++i) {

        for (int j = 0; j < X.ncol(); ++j) {

            if (S(index_s[i] - 1, j) > 0) {
                X(i, j) = xtest(0, j);
            } else {
                X(i, j) = xtrain(index_xtrain[i] - 1, j);
            }

        }
    }

    return X;
}
