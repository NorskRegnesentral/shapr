# Normalize mixed data for `vaeac`

Compute the mean and std for each continuous feature, while the
categorical features will have mean 0 and std 1.

## Usage

``` r
vaeac_normalize_data(
  data_torch,
  one_hot_max_sizes,
  norm_mean = NULL,
  norm_std = NULL
)
```

## Arguments

- one_hot_max_sizes:

  A torch tensor of dimension `n_features` containing the one hot sizes
  of the `n_features` features. That is, if the `i`th feature is a
  categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
  While the size for continuous features can either be `0` or `1`.

- norm_mean:

  Torch tensor (optional). A 1D array containing the means of the
  columns of `x_torch`.

- norm_std:

  Torch tensor (optional). A 1D array containing the stds of the columns
  of `x_torch`.

## Value

A list containing the normalized version of `x_torch`, `norm_mean` and
`norm_std`.

## Author

Lars Henry Berge Olsen
