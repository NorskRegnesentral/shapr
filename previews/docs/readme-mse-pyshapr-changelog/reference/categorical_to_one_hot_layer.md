# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a `categorical_to_one_hot_layer`

The `categorical_to_one_hot_layer` module/layer expands categorical
features into one-hot vectors, because multi-layer perceptrons are known
to work better with this data representation. It also replaces NaNs with
zeros in order so that further layers may work correctly.

## Usage

``` r
categorical_to_one_hot_layer(
  one_hot_max_sizes,
  add_nans_map_for_columns = NULL
)
```

## Arguments

- one_hot_max_sizes:

  A torch tensor of dimension `n_features` containing the one hot sizes
  of the `n_features` features. That is, if the `i`th feature is a
  categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
  While the size for continuous features can either be `0` or `1`.

- add_nans_map_for_columns:

  Optional list which contains indices of columns which is_nan masks are
  to be appended to the result tensor. This option is necessary for the
  full encoder to distinguish whether value is to be reconstructed or
  not.

## Details

Note that the module works with mixed data represented as 2-dimensional
inputs and it works correctly with missing values in `groundtruth` as
long as they are represented by NaNs.

## Author

Lars Henry Berge Olsen
