rm(list = ls())

devtools::load_all()

# Check that the given features are not resampled, but kept as is.
m <- 10
n_samples <- 50
mu <- rep(1, m)

set.seed(13)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))

# Checks full covariance matrix
isSymmetric(cov_mat) # TRUE
eigenvalues <- eigen(cov_mat, only.values = TRUE)$values
any(eigenvalues < 1e-08) # FALSE

# Sample test data
x_test <- MASS::mvrnorm(2, mu, cov_mat)[1, , drop = F] # sample_gaussian requires x_test to be a matrix

index_given <- 4:7
set.seed(1)
res3.1 <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = F)
set.seed(1)
res3.3 <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = T)
testthat::expect_false(sum(res3.1 != res3.3) == 0)
# Should not expect different results, since the conditional covariance matrix
# is symmetric and positive-definite.

# In the previous version this test was OK.
