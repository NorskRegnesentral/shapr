# Generate Gaussian MC samples

Generate Gaussian MC samples

## Usage

``` r
prepare_data_gaussian_cpp(MC_samples_mat, x_explain_mat, S, mu, cov_mat)
```

## Arguments

- MC_samples_mat:

  arma::mat. Matrix of dimension (`n_MC_samples`, `n_features`)
  containing samples from the univariate standard normal.

- x_explain_mat:

  arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing
  the observations to explain.

- S:

  arma::mat. Matrix of dimension (`n_coalitions`, `n_features`)
  containing binary representations of the used coalitions. S cannot
  contain the empty or grand coalition, i.e., a row containing only
  zeros or ones. This is not a problem internally in shapr as the empty
  and grand coalitions are treated differently.

- mu:

  arma::vec. Vector of length `n_features` containing the mean of each
  feature.

- cov_mat:

  arma::mat. Matrix of dimension (`n_features`, `n_features`) containing
  the covariance matrix of the features.

## Value

An arma::cube/3D array of dimension (`n_MC_samples`, `n_explain` \*
`n_coalitions`, `n_features`), where the columns (*,j,*) are matrices of
dimension (`n_MC_samples`, `n_features`) containing the conditional
Gaussian MC samples for each explicand and coalition.

## Author

Lars Henry Berge Olsen
