#include <RcppArmadillo.h>
using namespace Rcpp;

//' (Generalized) Mahalanobis distance
//'
//' Used to get the Euclidean distance as well by setting \code{mcov} = \code{diag(m)}.
//'
//' @param featureList List.
//' Contains the vectors indicating all factor combinations that should be included in the computations.
//' Assumes that the first one is empty.
//' @param Xtrain_mat Matrix
//' Training data in matrix form
//' @param Xexplain_mat Matrix
//' Explanation data in matrix form.
//'
//' @inheritParams hat_matrix_cpp
//' @keywords internal
//'
//' @return Array of three dimensions. Contains the squared distance for between all training and test observations for all feature combinations passed to the function.
//' @author Martin Jullum
// [[Rcpp::export]]
arma::cube mahalanobis_distance_cpp(Rcpp::List featureList,
                                    arma::mat Xtrain_mat,
                                    arma::mat Xexplain_mat,
                                    arma::mat mcov,
                                    bool S_scale_dist) {

    using namespace arma;

    // Define variables
    int ntrain = Xtrain_mat.n_rows;
    int ntest = Xexplain_mat.n_rows;
    int p = featureList.size();


    arma::mat mcov0;
    arma::mat cholDec;
    arma::mat mu0;
    arma::mat mu;
    arma::mat X;

    arma::cube out(ntrain,ntest,p,arma::fill::zeros);

    // Declaring some private variables

    double acc;
    uint32_t icol, irow, ii;
    double S_scale;
    IntegerVector temp;

    for (int k = 0; k < p; ++k){
        temp = featureList[k];
        if(temp.length() == 0) {
            continue;
        }
        arma::uvec theseFeatures = featureList[k];
        theseFeatures = theseFeatures-1;

        mcov0 = mcov.submat(theseFeatures,theseFeatures);
        X = Xtrain_mat.cols(theseFeatures);
        mu0 = Xexplain_mat.cols(theseFeatures);

        uint32_t d = X.n_cols;
        vec tmp(d);
        cholDec = trimatl(chol(mcov0).t());
        vec D = cholDec.diag();
        if(S_scale_dist){
            S_scale = 1.0/pow(theseFeatures.n_elem,2.0);
       } else {
            S_scale = 1.0;
        }

        for (int j = 0; j < ntest; ++j) {
            mu = mu0.row(j);

            // For each of the "n" random vectors, forwardsolve the corresponding linear system.
            // Forwardsolve because I'm using the lower triangle Cholesky.
            for(icol = 0; icol < ntrain; icol++)
            {

                for(irow = 0; irow < d; irow++)
                {
                    acc = 0.0;

                    for(ii = 0; ii < irow; ii++) acc += tmp.at(ii) * cholDec.at(irow, ii);

                    tmp.at(irow) = ( X.at(icol, irow) - mu.at(irow) - acc ) / D.at(irow);
                }

                out.at(icol,j,k) = sum(square(tmp));
            }

        }
        out.slice(k) *= S_scale;
    }


    return out;
}
