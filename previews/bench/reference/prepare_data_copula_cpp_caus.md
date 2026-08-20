# Generate (Gaussian) Copula MC samples for the causal setup with a single MC sample for each explicand

Generate (Gaussian) Copula MC samples for the causal setup with a single
MC sample for each explicand

## Usage

``` r
prepare_data_copula_cpp_caus(
  MC_samples_mat,
  x_explain_mat,
  x_explain_gaussian_mat,
  x_train_mat,
  S,
  mu,
  cov_mat
)
```

## Arguments

- MC_samples_mat:

  arma::mat. Matrix of dimension (`n_MC_samples`, `n_features`)
  containing samples from the univariate standard normal.

- x_explain_mat:

  arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing
  the observations to explain.

- x_explain_gaussian_mat:

  arma::mat. Matrix of dimension (`n_explain`, `n_features`) containing
  the observations to explain after being transformed using the Gaussian
  transform, i.e., the samples have been transformed to a standardized
  normal distribution.

- x_train_mat:

  arma::mat. Matrix of dimension (`n_train`, `n_features`) containing
  the training observations.

- S:

  arma::mat. Matrix of dimension (`n_coalitions`, `n_features`)
  containing binary representations of the used coalitions. S cannot
  contain the empty or grand coalition, i.e., a row containing only
  zeros or ones. This is not a problem internally in shapr as the empty
  and grand coalitions are treated differently.

- mu:

  arma::vec. Vector of length `n_features` containing the mean of each
  feature after being transformed using the Gaussian transform, i.e.,
  the samples have been transformed to a standardized normal
  distribution.

- cov_mat:

  arma::mat. Matrix of dimension (`n_features`, `n_features`) containing
  the pairwise covariance between all pairs of features after being
  transformed using the Gaussian transform, i.e., the samples have been
  transformed to a standardized normal distribution.

## Value

An arma::cube/3D array of dimension (`n_MC_samples`, `n_explain` \*
`n_coalitions`, `n_features`), where the columns (*,j,*) are matrices of
dimension (`n_MC_samples`, `n_features`) containing the conditional
Gaussian copula MC samples for each explicand and coalition on the
original scale.

## Author

Lars Henry Berge Olsen
