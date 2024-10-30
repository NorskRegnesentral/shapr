#define ARMA_WARN_LEVEL 1   // Deprecate "Matrix is singular, trying apporximate soulition" types of warnings
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat solve_cpp(arma::mat A, arma::mat b){
    // Using this function because it proceeds with the calculation even if the matrix is singular.
    // In practice, meaning that also small batch sizes can be used.
    return solve(A, b);
}

// [[Rcpp::export]]
arma::mat create_A_new_cpp(const arma::mat& S, const arma::mat & W){
    int num_players = S.n_cols;
    int batch_size = S.n_rows;
    int batch_weight = accu(W);

    arma::mat A_sample = arma::mat(num_players, num_players).fill(0.);
    for (int j = 0; j < batch_size; j++){
        A_sample = A_sample + W(j, j) * S.row(j).t() * S.row(j) / batch_weight;
    }
    return A_sample;
}