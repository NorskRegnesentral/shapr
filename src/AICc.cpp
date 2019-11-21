#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Computing single H matrix in AICc-function using the Mahalanobis distance
//'
//' @param X matrix with "covariates"
//' @param mcov covariance matrix
//' @param S_scale_dist logical indicating whether the Mahalanobis distance should be scaled with the number of variables
//' @param h numeric specifying the scaling (sigma)
//'
//' @export
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

//' sigma_hat_sq-function
//'
//' @param H Matrix. Output from \code{\link{hat_matrix_cpp}}
//' @param y Vector, i.e. representing the response variable
//'
//' @export
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

//' correction term with trace_input in AICc formula
//'
//' @param tr_H numeric giving the trace of H
//' @param n numeric given the number of rows in H
//' @export
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
//' @param X matrix with "covariates"
//' @param mcov covariance matrix
//' @param S_scale_dist logical indicating whether the Mahalanobis distance should be scaled with the number of variables
//' @param h numeric specifying the scaling (sigma)
//' @param y vector with the "response variable"
//'
//' @export
//'
//' @return Scalar with the numeric value of the AICc formula
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
//' @param h Numeric. Specifies the scaling (sigma)
//' @param X_list List
//' @param mcov_list List
//' @param S_scale_dist Logical. Indicates whether Mahalanobis distance should be scaled with the
//' number of variables
//' @param y_list List.
//' @param negative Logical.
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

        out += log(summer(0)/summer(2)) + correction_matrix_cpp(summer(1),summer(2)); // This computes log(sigma_1 = rss_1/n_1) + correction_formula(H = H_1,n = n_1) + log(sigma_2 = rss_2/n_2) + correction_formula(H = H_2,n = n_2)
    }

    if(negative){
        out *= -1;
    }

    return(out);
}

// //' sigma_hat_sq-function
// //'
// //' @param H matrix being the output from hat_matrix_cpp
// //' @param y vector with the "response variable"
// //' @param ret_log logical indicating whether to return the logarithm of the sigma_sq
// //' @export
// //'
// //' @return Scalar
// //' @author Martin Jullum
// // [[Rcpp::export]]
// double sigma_hat_sq_cpp(arma::mat H, arma::vec y, bool ret_log) {
//
//     // Define variables
//     int n = y.n_elem;
//     arma::mat one(n,n,fill::zeros);
//     arma::vec two(n,fill::zeros);
//     double out;
//
//     arma::mat diag(n,n,fill::eye);
//
//     // simplifies
//     one = diag - H;
//     two = one*y;
//
//     out = as_scalar(two.t()*two)/n;
//
//     if(ret_log){
//         out = log(out);
//     }
//
//     return(out);
// }

// //'  AICc formula for several sets
// //'
// //' @param h numeric specifying the scaling (sigma)
// //' @param X matrix with "covariates"
// //' @param mcov covariance matrix
// //' @param S_scale_dist logical indicating whether the Mahalanobis distance should be scaled with the number of variables
// //' @param y vector with the "response variable"
// //'
// //' @export
// //'
// //' @return Scalar with the numeric value of the AICc formula
// //' @author Martin Jullum
// // [[Rcpp::export]]
// double AICc_full_cpp(double h, Rcpp::List X_list, Rcpp::List mcov_list, bool S_scale_dist, Rcpp::List y_list, bool negative) {
//
//     int nloops = X_list.size();
//     arma::vec summer(3,fill::zeros);
//
//     for (int k = 0; k < nloops; ++k){
//         arma::mat X = X_list[k];
//         arma::mat mcov = mcov_list[k];
//         arma::vec y = y_list[k];
//
//         summer += aicc_full_single_cpp(X, mcov, S_scale_dist, h,  y);
//     }
//
//     double out = log(summer(0)/summer(2)) + correction_matrix_cpp(summer(1),summer(2)); // This computes log(sigma_full = (rss_1 + rss_2)/(n_1+n_2)) + correction_formula(H = H_1+H_2, n = n_1 + n_2)
//
//     if(negative){
//         out *= -1;
//     }
//
//     return(out);
// }


//
// //' correction term in AICc formula
// //'
// //' @param H matrix being the output from hat_matrix_cpp
// //' @export
// //'
// //' @return Scalar
// //' @author Martin Jullum
// // [[Rcpp::export]]
// double correction_term_cpp(arma::mat H) {
//
//     // Define variables
//     double out;
//
//     double n = H.n_rows;
//     double tr = trace(H);
//
//     out = (1.0+tr/n)/(1.0-(tr+2.0)/n);
//
//     return(out);
// }
//
//
//
// //'  AICc formula for single X
// //'
// //' @param X matrix with "covariates"
// //' @param mcov covariance matrix
// //' @param S_scale_dist logical indicating whether the Mahalanobis distance should be scaled with the number of variables
// //' @param h numeric specifying the scaling (sigma)
// //' @param y vector with the "response variable"
// //'
// //' @export
// //'
// //' @return Scalar with the numeric value of the AICc formula
// //' @author Martin Jullum
// // [[Rcpp::export]]
// double AICc_single_cpp(arma::mat X, arma::mat mcov, bool S_scale_dist, double h, arma::vec y, bool negative) {
//
//     bool ret_log = true;
//
//     arma::mat H = hat_matrix_cpp(X, mcov, S_scale_dist, h);
//
//
//     double log_sigma_hat_sq = sigma_hat_sq_cpp(H, y, ret_log);
//     double correction = correction_term_cpp(H);
//
//     double out = log_sigma_hat_sq + correction;
//
//     if(negative){
//         out *= -1;
//     }
//
//     return(out);
// }




// //' NEW Computing single H matrix in AICc-function using the Mahalanobis distance
// //'
// //' @param X matrix with "covariates"
// //' @param mcov covariance matrix
// //' @param S_scale_dist logical indicating whether the Mahalanobis distance should be scaled with the number of variables
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
//     arma::mat out1 = hat_matrix_cpp(X, mcov_list[0], S_scale_dist, h);
// //    out2 = hat_matrix_cpp(X, mcov, S_scale_dist, h);
//
//     std::vector<arma::mat> out;
//
//     out[0] = out1;
//
//     out1 = out[0]+1;
//
//     return(out1);
// }


