devtools::load_all()
library(data.table)
library(MASS)

n_features = 12
n_coalitions = 100

X = create_coalition_table(n_features, exact = FALSE, n_coalitions = n_coalitions)
S = coalition_matrix_cpp(coalitions = X[["features"]], m = n_features)

vS = as.data.table(rnorm(nrow(S)))
vS[, id_coalition := X[, id_coalition]]

p0 = 3
preds = 1

n_row_all = n_row_this_iter = sum(X[-c(1, .N), shapley_weight])

# Comparing R and C++ implementations for computing A_new

library(Rcpp)
Rcpp::sourceCpp("R/frida_utils.cpp")


A_new = matrix(0, nrow = n_features, n_features)
for (i in 2:(nrow(S) - 1)) {
    A_new = A_new + X[i, shapley_weight] * S[i, ]%*%t(S[i, ])/n_row_this_iter
}

SS = S[2:(nrow(S) - 1), ]
W = diag(X[2:(nrow(S) - 1), shapley_weight])
n_row_this_iter = sum(W)
A_new_cpp = create_A_new_cpp(SS, W)

A_new - A_new_cpp




A = compute_A(0, X, S, n_row_all, n_row_this_iter)

b = compute_b(0, vS, X, S, n_row_all, n_row_this_iter, p0)

# Compute Shapley values
n_features = ncol(A)

A_inv_one = solve(A, rep(1, n_features))
A_inv_vec = solve(A, t(b))

numerator = colSums(A_inv_vec) - (preds - p0)
numerator = matrix(rep(as.numeric(numerator), n_features), nrow = n_features, byrow = TRUE)

shapley_values = A_inv_vec - A_inv_one * numerator / sum(A_inv_one)

A = as.matrix(A)
b = as.matrix(b)

library(Rcpp)
Rcpp::sourceCpp("R/frida_utils.cpp")
# vals_cpp = calculate_shapley_values_cpp(A, b, preds, p0)
# vals_cpp

A_inv_one_cpp = solve_cpp(A, as.matrix(rep(1, n_features)))
A_inv_vec_cpp = solve_cpp(A, t(b))
