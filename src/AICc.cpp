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
//' @author Martin Jullum
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

    uint32_t d = m;
    vec tmp(d);
    cholDec = trimatl(chol(mcov).t()); // Should also compute this outside of the function and pass it directly.
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

//' sigma_hat_sq-function
//'
//' @param H matrix being the output from H_cpp
//' @param y vector with the "response variable"
//' @param ret_log logical indicating whether to return the logarithm of the sigma_sq
//' @export
//'
//' @return Scalar
//' @author Martin Jullum
// [[Rcpp::export]]
double sigma_hat_sq_cpp(arma::mat H, arma::vec y, bool ret_log) {

    // Define variables
    int n = y.n_elem;
    arma::mat one(n,n,fill::zeros);
    arma::vec two(n,fill::zeros);
    double out;

    arma::mat diag(n,n,fill::eye);

    // simplifies
    one = diag - H;
    two = one*y;

    out = as_scalar(two.t()*two)/n;

    if(ret_log){
        out = log(out);
    }

    return(out);
}

//' correction term in AICc formula
//'
//' @param H matrix being the output from H_cpp
//' @export
//'
//' @return Scalar
//' @author Martin Jullum
// [[Rcpp::export]]
double correction_term_cpp(arma::mat H) {

    // Define variables
    double out;

    double n = H.n_rows;
    double tr = trace(H);

    out = (1.0+tr/n)/(1.0-(tr+2.0)/n);

    return(out);
}





// //' NEW Computing single H matrix in AICc-function using the Mahalanobis distance
// //'
// //' @param X matrix with "covariates"
// //' @param mcov covariance matrix
// //' @param S_scale_dist logical indicating whether the Mahlanobis distance should be scaled with the number of variables
// //' @param h numeric specifying the scaling (sigma)
// //'
// //' @export
// //'
// //' @return Matrix of dimension ncol(X) \times ncol(X)
// //' @author Martin Jullum
// // [[Rcpp::export]]
// std::vector<arma::mat> H_new_cpp(Rcpp::List X_list, Rcpp::List mcov_list, bool S_scale_dist, double h) {
//
//     // Define variables
//     arma::mat X = X_list[0];
//
//     int nrows = X.n_rows;
//     int m = X.n_cols;
//
//     arma::mat cholDec;
//     arma::mat mu;
//
// //    arma::mat out1(nrows,nrows,arma::fill::zeros);
// //    arma::mat out2(nrows,nrows,arma::fill::zeros);
//
//
//     arma::mat out1 = H_cpp(X, mcov_list[0], S_scale_dist, h);
// //    out2 = H_cpp(X, mcov, S_scale_dist, h);
//
//     std::vector<arma::mat> out;
//
//     out[0] = out1;
//
//     out1 = out[0]+1;
//
//     return(out1);
// }


