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
arma::cube prepare_gen_Mahlanobis_dist_cpp_old(arma::mat Xtrain, arma::mat Xtest, arma::mat mcov) {

    using namespace arma;

    // Define variables
    int ntrain = Xtrain.n_rows;
    int ntest = Xtest.n_rows;
    int m = Xtrain.n_cols;

    arma::mat cholDec;

    cholDec = trimatl(chol(mcov).t());

    vec D = cholDec.diag();

    //vec out(Xtrain.n_rows);
    arma::cube out(ntrain,ntest,m,arma::fill::zeros);

    // Declaring some private variables
    uint32_t d = Xtrain.n_cols;
    vec tmp(d);

    double acc;
    uint32_t icol, irow, ii;
    arma::mat mu;

    for (int j = 0; j < ntest; ++j) {
        //mu = conv_to<vec>::from(Xtest.row(j)); // Not sure if this is needed
        mu = Xtest.row(j); // Not sure if this is needed

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

            out.tube(icol,j) = square(tmp).t(); // ?

        }
    }

    return out;


    /* #Equivalent R-code
     for (i in Xtest[,.I]){ # Rewrite to Rcpp
     dec <- chol(mcov)
     D[,i,] <- t(forwardsolve(t(dec), t(as.matrix(l$Xtrain)) - unlist(l$Xtest[i,]) )^2)
     }
     */

    }


//' (Generalized) Mahalanobis distance
//'
//' Used to get the Euclidean distance as well by setting mcov = diag(m).
//' @param featureList List of vectors indicating all facture combinations that should be included in the computations. Assumes that the first one is empty.
//' @param Xtrain Dataframe
//' @param Xtest Dataframe
//' @param mcov Matrix. The Sigma-matrix in the Mahalanobis distance formula (cov(Xtrain) gives Mahalanobis distance,
//' diag(m) gives the Euclidean distance.
//' @param S_scale_dist Logical indicating
//'
//' @export
//'
//' @return Array of three dimensions containg the the squared distance for between all training and test observations for all feature combinations passed to the function.
//' @author Martin Jullum
// [[Rcpp::export]]
arma::cube gen_Mahlanobis_dist_cpp(Rcpp::List featureList,arma::mat Xtrain, arma::mat Xtest, arma::mat mcov, bool S_scale_dist, bool normalize_rows) {

    using namespace arma;

    // Define variables
    int ntrain = Xtrain.n_rows;
    int ntest = Xtest.n_rows;
    int m = Xtrain.n_cols;
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
    double rowsum;


    for (int k = 1; k < p; ++k){ // Ignoring the first List element (assuming it contains en empty vector)

        arma::uvec theseFeatures = featureList[k];
        theseFeatures = theseFeatures-1;

        mcov0 = mcov.submat(theseFeatures,theseFeatures);
        X = Xtrain.cols(theseFeatures);
        mu0 = Xtest.cols(theseFeatures);

        uint32_t d = X.n_cols;
        vec tmp(d);
        cholDec = trimatl(chol(mcov0).t());
        vec D = cholDec.diag();
        if(S_scale_dist){
            S_scale = 1.0/pow(theseFeatures.n_elem,2);
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
            // testing new stuff //// FOR NOW JUST DO THIS IN R INSTEAD !!!!!!
            if(normalize_rows){
                // gettign the sum in an akward way
                for(icol = 0; icol < ntrain; icol++){
                    rowsum +=out.at(icol,j,k);
                }

// old non-working code     rowsum = sum(sub2ind()out.subcube(0,j,k,out.n_rows,j,k)));
                for(icol = 0; icol < ntrain; icol++){
                    out.at(icol,j,k) *= 1.0/rowsum;
                }
            }

            // end testing new stuff


        }
        out.slice(k) *= S_scale;
    }


    return out;
}


