#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Computing single H matrix in AICc-function using the Mahalanobis distance
//'
//' @param X matrix.
//' @param mcov matrix
//' The covariance matrix of X.
//' @param S_scale_dist logical.
//' Indicating whether the Mahalanobis distance should be scaled with the number of variables
//' @param h numeric specifying the scaling (sigma)
//'
//' @keywords internal
//'
//' @return Matrix of dimension \code{ncol(X)*ncol(X)}
//' @author Martin Jullum
// [[Rcpp::export]]
arma::mat hat_matrix_cpp(arma::mat X, arma::mat mcov, bool S_scale_dist, double h) {


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

    out = normalise(out,1,1);

    return out;
}

//' Function for computing sigma_hat_sq
//'
//' @param H Matrix.
//' Output from [hat_matrix_cpp()]
//' @param y Vector
//' Representing the (temporary) response variable
//'
//' @keywords internal
//'
//' @return Scalar
//'
//' @author Martin Jullum
// [[Rcpp::export]]
double rss_cpp(arma::mat H, arma::vec y) {

    // Define variables
    int n = y.n_elem;
    arma::mat one(n,n,fill::zeros);
    arma::vec two(n,fill::zeros);
    double out;

    arma::mat diag(n,n,fill::eye);

    // simplifies
    one = diag - H;
    two = one*y;

    out = as_scalar(two.t()*two);

    return(out);
}

//' Correction term with trace_input in AICc formula
//'
//' @param tr_H numeric
//' The trace of H
//' @param n numeric
//' The number of rows in H
//'
//' @keywords internal
//'
//' @return Scalar
//' @author Martin Jullum
// [[Rcpp::export]]
double correction_matrix_cpp(double tr_H,int n) {

    double out = (1.0+tr_H/n)/(1.0-(tr_H+2.0)/n);

    return(out);
}

//'  Temp-function for computing the full AICc with several X's etc
//'
//'
//' @inheritParams hat_matrix_cpp
//' @inheritParams rss_cpp
//'
//' @keywords internal
//'
//' @return Scalar with the numeric value of the AICc formula.
//' @author Martin Jullum
// [[Rcpp::export]]
arma::vec aicc_full_single_cpp(arma::mat X, arma::mat mcov, bool S_scale_dist, double h, arma::vec y) {

    arma::vec out(3);

    arma::mat H = hat_matrix_cpp(X, mcov, S_scale_dist, h);

    double rss = rss_cpp(H, y);
    double tr_H = trace(H);

    out(0) = rss;
    out(1) = tr_H;
    out(2) = y.n_elem;

    return(out);
}

//'  AICc formula for several sets, alternative definition
//'
//' @param X_list List.
//' Contains matrices with the appropriate features of the training data
//' @param mcov_list List.
//' Contains the covariance matrices of the matrices in X_list
//' @param S_scale_dist Logical.
//' Indicates whether Mahalanobis distance should be scaled with the number of variables.
//' @param y_list List.
//' Contains the appropriate (temporary) response variables.
//' @param negative Logical.
//' Whether to return the negative of the AICc value.
//'
//' @keywords internal
//'
//' @inheritParams hat_matrix_cpp
//'
//' @return Scalar with the numeric value of the AICc formula
//'
//' @author Martin Jullum
// [[Rcpp::export]]
double aicc_full_cpp(double h, Rcpp::List X_list, Rcpp::List mcov_list, bool S_scale_dist, Rcpp::List y_list, bool negative) {

    int nloops = X_list.size();
    arma::vec summer(3,fill::zeros);
    double out = 0.0;

    for (int k = 0; k < nloops; ++k){
        arma::mat X = X_list[k];
        arma::mat mcov = mcov_list[k];
        arma::vec y = y_list[k];

        summer = aicc_full_single_cpp(X, mcov, S_scale_dist, h,  y);

        // This computes log(sigma_1 = rss_1/n_1) + correction_formula(H = H_1,n = n_1) + log(sigma_2 = rss_2/n_2) + correction_formula(H = H_2,n = n_2)
        out += log(summer(0)/summer(2)) + correction_matrix_cpp(summer(1),summer(2));
    }

    if(negative){
        out *= -1;
    }

    return(out);
}
