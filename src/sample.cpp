#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param D 3-D array
//' @param ncomb Positive integer. Total number of combinations
//' @inheritParams global_arguments
//'
//' @export
//'
//' @return List
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<int> sample_cpp(arma::Cube<double> D, int nsamples, int ncomb) {

    // Setup
    int ntrain, ntest;
    ntrain = D.n_rows;
    ntest = D.n_cols;
    arma::Cube<int> Y(nsamples, ntest, ncomb);
    std::default_random_engine generator;
    NumericVector weights(ntrain);

    // Loop through all combinations
    for (int k = 0; k < ncomb; ++k) {

        // Loop through test data
        for (int j = 0; j < ntest; ++j) {

            // Get weights
            for (int i = 0; i < ntrain; ++i) {
                weights[i] = arma::as_scalar(D(arma::span(i), arma::span(j), arma::span(k)));
            }

            // Define sampler
            std::discrete_distribution<int> distribution(weights.begin(), weights.end());

            for (int i = 0; i < nsamples; ++i) {
                int number = distribution(generator);
                Y(i, j, k) = number + 1;
            }
        }
    }

    return Y;
}

//' Get distance
//'
//' @param D 3-D array
//' @param ncomb Positive integer. Total number of combinations
//' @inheritParams global_arguments
//'
//' @export
//'
//' @return List
//' @author Nikolai Sellereite
// [[Rcpp::export]]
IntegerMatrix sample_unit(NumericMatrix D, List features, int nsamples, double sigma) {

    // Define variables
    int ntrain, ncomb;
    ntrain = D.nrow();
    ncomb = features.length();

    // Sample from training data
    int nfeatures;
    NumericVector feature_vec;
    NumericVector weights(ntrain);
    std::fill(weights.begin(), weights.end(), 1.0);
    IntegerMatrix X(ntrain, ncomb);
    std::default_random_engine generator;


    for (int j = 0; j < ncomb; ++j) {

        feature_vec = features[j];
        nfeatures = feature_vec.length();
        if(nfeatures > 0) {

            std::fill(weights.begin(), weights.end(), 0.0);
            for (int i = 0; i < nfeatures; ++i) {
                weights += D(_, feature_vec[i] - 1);
            }

            weights = sqrt(exp((-0.5 * weights) / pow(sigma, 2.0)));

        } else {
            std::fill(weights.begin(), weights.end(), 1.0);
        }

        // Define sampler
        std::discrete_distribution<int> distribution(weights.begin(), weights.end());

        for (int i = 0; i < nsamples; ++i) {
            int number = distribution(generator);
            ++X(number, j);
        }
    }

    return X;
}

//' Get distance
//'
//' @param D 3-D array
//' @param ncomb Positive integer. Total number of combinations
//' @inheritParams global_arguments
//'
//' @export
//'
//' @return List
//' @author Nikolai Sellereite
// [[Rcpp::export]]
NumericMatrix impute_data_unit(NumericMatrix D, NumericMatrix Xtrain, NumericMatrix Xtest) {

    // Count how many rows we need
    int N = 0;
    for (int j = 0; j < D.ncol(); ++j) {

         N += sum((D(_, j) > 0) * 1);
    }

    NumericMatrix X(N, Xtrain.ncol() + 3);

    for (int i = 0; i < N; ++i) {

        for (int j = 0; j < D.ncol(); ++j) {

            for (int s = 0; s < D.nrow(); ++s) {

                X(i, j) = 1;
            }
        }
    }

    return X;
}

//' Get distance
//'
//' @export
//'
//' @author Nikolai Sellereite
// [[Rcpp::export]]
NumericVector test(NumericMatrix D, IntegerVector a) {

    std::default_random_engine generator;
    std::discrete_distribution<int> distribution {0.1, 0.2, 0.3, 0.4};

    NumericVector X(4);
    for(int i = 0; i < 1000; ++i) {
        int num = distribution(generator);
        ++X[num];
    }

    return X;

}
