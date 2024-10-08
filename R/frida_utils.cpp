#define ARMA_WARN_LEVEL 1   // Deprecate "Matrix is singular, trying apporximate soulition" types of warnings
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

arma::mat solve_cpp(arma::mat A, arma::mat b){
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

// // [[Rcpp::depends(RcppArmadillo)]]

// // [[Rcpp::export]]
// arma::mat calculate_shapley_values_cpp(arma::mat A, arma::mat b, float preds, float p0){
//     int n_features = A.n_cols;

//     arma::vec ones = arma::ones<arma::vec>(n_features);

//     arma::mat A_inv_one = solve(A, ones);
//     // std::cout << "A_inv_one" << A_inv_one << std::endl;
//     // std::cout << "A" << A << std::endl;
//     // std::cout << A.n_rows << " " << A.n_cols << std::endl;
//     // std::cout << "b" << b << std::endl;
//     // std::cout << b.n_rows << " " << b.n_cols << std::endl;
//     arma::mat A_inv_vec = solve(A, b.t());
//     // std::cout << "A_inv_vec" << A_inv_vec << std::endl;
//     // std::cout << "preds" << preds << std::endl;
//     // std::cout << "p0" << p0 << std::endl;
//     // std::cout << "sum(A_inv_vec, 1)" << sum(A_inv_vec, 1) << std::endl;
//     arma::mat numerator = sum(A_inv_vec, 0) - (preds - p0);
//     // std::cout << numerator << std::endl;
//     numerator = repmat(numerator, 1, n_features).t();
//     // std::cout << numerator << std::endl;

//     // A_inv_one = repmat(A_inv_one, 1, n_features).t();

//     // std::cout << A_inv_vec.n_rows << " " << A_inv_vec.n_cols << std::endl;
//     // std::cout << A_inv_one.n_rows << " " << A_inv_one.n_cols << std::endl;
//     // std::cout << numerator.n_rows << " " << numerator.n_cols << std::endl;
//     std::cout << A_inv_one << std::endl;
//     std::cout << A_inv_vec << std::endl;
//     std::cout << sum(A_inv_one) << std::endl;
//     std::cout << numerator << std::endl;

//     // float denom = sum(A_inv_one); // (0,0);
//     arma::mat shapley_values = A_inv_vec - A_inv_one % numerator/sum(A_inv_one);

//     // return shapley_values;
//     return A_inv_vec;
// }