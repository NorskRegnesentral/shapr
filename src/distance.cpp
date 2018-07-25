#include <RcppArmadillo.h>
using namespace Rcpp;

//' Get distance
//'
//' @param Xtrain Dataframe
//' @param Xtest Dataframe
//'
//' @export
//'
//' @return Array of three dimensions
//' @author Nikolai Sellereite
// [[Rcpp::export]]
arma::Cube<double> distance_cpp(NumericMatrix Xtrain, NumericMatrix Xtest) {

    // Define variables
    int ntrain, ntest, nfeatures;
    ntrain = Xtrain.nrow();
    ntest = Xtest.nrow();
    nfeatures = Xtrain.ncol();
    arma::cube X(ntrain, ntest, nfeatures, arma::fill::zeros);

    for (int k = 0; k < nfeatures; ++k) {

        for (int j = 0; j < ntest; ++j) {

            for (int i = 0; i < ntrain; ++i) {

                X(i, j, k) = pow(Xtrain(i, k) - Xtest(j, k), 2.0);
            }

        }
    }

    return X;
}

//' Prepare (generalized) Mahalanobis distance
//'
//' Used to get the Euclidean distance as well by setting mcov = diag(m).
//' @param Xtrain Dataframe
//' @param Xtest Dataframe
//' @param mcov Matrix. The Sigma-matrix in the Mahalanobis distance formula (cov(Xtrain) gives Mahalanobis distance,
//' diag(m) gives the Euclidean distance.
//'
//' @export
//'
//' @return Array of three dimensions. Multiplying X[,i,] with t(S) gives the Mahlanbis distance for all combinations
//' @author Martin Jullum
// [[Rcpp::export]]
// <double>
arma::mat prepare_gen_Mahlanobis_dist_cpp(arma::mat Xtrain, arma::mat Xtest, arma::mat mcov) {

    using namespace arma;

    // Define variables
    int ntrain, ntest, nfeatures;
    ntrain = Xtrain.n_rows;

 //   ntest = Xtest.nrow();
 //   nfeatures = Xtrain.ncol();
    arma::cube X(ntrain, ntest, nfeatures, arma::fill::zeros);
    arma::mat cholDec;

    cholDec = trimatl(chol(mcov).t());

//  arma::vec mahaInt(arma::mat & X,
//                    arma::vec & mu,
//                      arma::mat & sigma,
//                      const bool isChol = false)

        vec D = cholDec.diag();

        //vec out(Xtrain.n_rows);
        arma::mat out(Xtrain.n_rows,Xtrain.n_cols);

        // Declaring some private variables
        uint32_t d = Xtrain.n_cols;
        vec tmp(d);

        double acc;
        uint32_t icol, irow, ii;
        arma::vec mu;

        //for (int j = 0; j < ntest; ++j) {
            mu = conv_to<vec>::from(Xtest.row(0)); // Not sure if this is needed

        // For each of the "n" random vectors, forwardsolve the corresponding linear system.
        // Forwardsolve because I'm using the lower triangle Cholesky.
        for(icol = 0; icol < ntrain; icol++)
        {

            for(irow = 0; irow < d; irow++)
            {
                acc = 0.0;

                for(ii = 0; ii < irow; ii++) acc += tmp.at(ii) * cholDec.at(irow, ii);

                tmp.at(irow) = ( Xtrain.at(icol, irow) - mu.at(irow) - acc ) / D.at(irow);
            }

            out.row(icol) = square(tmp).t();
        }

        return out;


        /* #Equivalent R-code
         for (i in Xtest[,.I]){ # Rewrite to Rcpp
         dec <- chol(mcov)
         D[,i,] <- t(forwardsolve(t(dec), t(as.matrix(l$Xtrain)) - unlist(l$Xtest[i,]) )^2)
         }
         */

    }




