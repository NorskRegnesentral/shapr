# Sample ctree variables from a given conditional inference tree

Sample ctree variables from a given conditional inference tree

## Usage

``` r
sample_ctree(tree, n_MC_samples, x_explain, x_train, n_features, sample)
```

## Arguments

- tree:

  List. Contains tree which is an object of type ctree built from the
  party package. Also contains given_ind, the features to condition
  upon.

- n_MC_samples:

  Scalar integer. Corresponds to the number of samples from the leaf
  node. See an exception when sample = FALSE in
  [`setup_approach.ctree()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md).

- x_explain:

  Data.table with the features of the observation whose predictions
  ought to be explained (test data).

- x_train:

  Data.table with training data.

- n_features:

  Positive integer. The number of features.

## Value

data.table with `n_MC_samples` (conditional) Gaussian samples

## Details

See the documentation of the
[`setup_approach.ctree()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)
function for undocumented parameters.

## Author

Annabelle Redelmeier
