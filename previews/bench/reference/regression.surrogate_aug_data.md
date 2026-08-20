# Augment the training data and the explicands

Augment the training data and the explicands

## Usage

``` r
regression.surrogate_aug_data(
  internal,
  x,
  y_hat = NULL,
  index_features = NULL,
  augment_masks_as_factor = FALSE,
  augment_include_grand = FALSE,
  augment_add_id_coal = FALSE,
  augment_comb_prob = NULL,
  augment_weights = NULL
)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

- x:

  Data.table containing the training data.

- y_hat:

  Vector of numerics (optional) containing the predicted responses for
  the observations in `x`.

- index_features:

  Array of integers (optional) containing which coalitions to consider.
  Must be provided if `x` is the explicands.

- augment_masks_as_factor:

  Logical (default is `FALSE`). If `TRUE`, then the binary masks are
  converted to factors. If `FALSE`, then the binary masks are numerics.

- augment_include_grand:

  Logical (default is `FALSE`). If `TRUE`, then the grand coalition is
  included. If `index_features` are provided, then
  `augment_include_grand` has no effect. Note that if we sample the
  coalitions then the grand coalition is equally likely to be sampled as
  the other coalitions (or weighted if `augment_comb_prob` is provided).

- augment_add_id_coal:

  Logical (default is `FALSE`). If `TRUE`, an additional column is
  adding containing which coalition was applied.

- augment_comb_prob:

  Array of numerics (default is `NULL`). The length of the array must
  match the number of coalitions being considered, where each entry
  specifies the probability of sampling the corresponding coalition.
  This is useful if we want to generate more training data for some
  specific coalitions. One possible choice would be
  `augment_comb_prob = if (use_Shapley_weights) internal$objects$X$shapley_weight[2:actual_n_coalitions] else NULL`.

- augment_weights:

  String (optional). Specifying which type of weights to add to the
  observations. If `NULL` (default), then no weights are added. If
  `"Shapley"`, then the Shapley weights for the different coalitions are
  added to corresponding observations where the coalition was applied.
  If `uniform`, then all observations get an equal weight of one.

## Value

A data.table containing the augmented data.

## Author

Lars Henry Berge Olsen
