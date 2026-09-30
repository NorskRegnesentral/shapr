# Generate marginal Gaussian data using Cholesky decomposition

Given a multivariate Gaussian distribution, this function creates data
from specified marginals of said distribution.

## Usage

``` r
create_marginal_data_gaussian(n_MC_samples, Sbar_features, mu, cov_mat)
```

## Arguments

- n_MC_samples:

  Integer. The number of samples to generate.

- Sbar_features:

  Vector of integers indicating which marginals to sample from.

- mu:

  Numeric vector containing the expected values for all features in the
  multivariate Gaussian distribution.

- cov_mat:

  Numeric matrix containing the covariance between all features in the
  multivariate Gaussian distribution.

## Author

Lars Henry Berge Olsen
