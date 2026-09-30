# Compute Featurewise Means and Standard Deviations

Returns the means and standard deviations for all continuous features in
the data set. Categorical features get \\mean = 0\\ and \\sd = 1\\ by
default.

## Usage

``` r
vaeac_compute_normalization(data, one_hot_max_sizes)
```

## Arguments

- data:

  A torch_tensor of dimension `n_observation` x `n_features` containing
  the data.

- one_hot_max_sizes:

  A torch tensor of dimension `n_features` containing the one hot sizes
  of the `n_features` features. That is, if the `i`th feature is a
  categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
  While the size for continuous features can either be `0` or `1`.

## Value

List containing the means and the standard deviations of the different
features.

## Author

Lars Henry Berge Olsen
