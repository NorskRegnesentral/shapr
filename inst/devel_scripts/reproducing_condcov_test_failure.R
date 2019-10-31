
rm(list=ls())
library(shapr)
# Check that the given features are not resampled, but kept as is.
m <- 10
n_samples <- 50
mu <- rep(1, m)

for (i in 1:100){

  this.seed = i
  set.seed(i) # Setting seed to avoid sampling a cov_mat which is exactly symmetric in certain dimensions,
  # such that ensure_condcov_symmetry would not affect the results
  cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
  x_test <- MASS::mvrnorm(2, mu, cov_mat)[1,,drop=F] # sample_gaussian requires x_test to be a matrix

  # Example 3 -------------
  # Check that ensuring conditional covariance matrix symmetry is FALSE by default.
  index_given <- 4:7
  set.seed(1)
  res3.1 <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = F)
  set.seed(1)
  res3.3 <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = T)

  print(c(i,sum(res3.1 != res3.3)))

}

set.seed(13) # Setting seed to avoid sampling a cov_mat which is exactly symmetric in certain dimensions,
# such that ensure_condcov_symmetry would not affect the results
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- MASS::mvrnorm(2, mu, cov_mat)[1,,drop=F] # sample_gaussian requires x_test to be a matrix

# Example 3 -------------
# Check that ensuring conditional covariance matrix symmetry is FALSE by default.
index_given <- 4:7
set.seed(1)
res3.1 <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = F)
set.seed(1)
res3.3 <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test, ensure_condcov_symmetry = T)


testthat::expect_false(sum(res3.1 != res3.3) == 0) # Expect different results
