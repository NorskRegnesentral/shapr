#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Computing single H matrix in AICc-function using the Mahalanobis distance
//'
//' @param X matrix with "covariates"
//' @param mcov covariance matrix
//' @param S_scale_dist logical indicating whether the Mahlanobis distance should be scaled with the number of variables
//' @param h numeric specifying the scaling (sigma)
//'
//' @export
//'
//' @return Matrix of dimension ncol(X) \times ncol(X)
//' @author Nikolai Sellereite
// [[Rcpp::export]]

arma::mat H_cpp(arma::mat X, arma::mat mcov, bool S_scale_dist, double h) {


    // Define variables
    int nrows = X.n_rows;
    int m = X.n_cols;

    arma::mat cholDec;
    arma::mat mu;

    arma::mat out(nrows,nrows,arma::fill::zeros);

    // Declaring some private variables

    double acc;
    uint32_t icol, irow, ii;
    double S_scale;
    double rowsum;

    uint32_t d = m;
    vec tmp(d);
    cholDec = trimatl(chol(mcov).t());
    vec D = cholDec.diag();
    if(S_scale_dist){
        S_scale = 0.5/pow(m*h,2);
    } else {
        S_scale = 0.5/pow(h,2);
    }

    for (int j = 0; j < nrows; ++j) {
        mu = X.row(j);

        // For each of the "n" random vectors, forwardsolve the corresponding linear system.
        // Forwardsolve because Im using the lower triangle Cholesky.
        for(icol = 0; icol < nrows; icol++)
        {

            for(irow = 0; irow < d; irow++)
            {
                acc = 0.0;

                for(ii = 0; ii < irow; ii++) acc += tmp.at(ii) * cholDec.at(irow, ii);

                tmp.at(irow) = ( X.at(icol, irow) - mu.at(irow) - acc ) / D.at(irow);
            }

            out.at(icol,j) = sum(square(tmp));

        }

    }
    out *= S_scale;

    out = exp(-out);

    return out;
}
