# Get all coalitions satisfying the causal ordering

This function is only relevant when we are computing asymmetric Shapley
values. For symmetric Shapley values (both regular and causal), all
coalitions are allowed.

## Usage

``` r
get_valid_causal_coalitions(
  causal_ordering,
  sort_features_in_coalitions = TRUE
)
```

## Arguments

- causal_ordering:

  List. Not applicable for (regular) non-causal or asymmetric
  explanations. `causal_ordering` is an unnamed list of vectors
  specifying the components of the partial causal ordering that the
  coalitions must respect. Each vector represents a component and
  contains one or more features/groups identified by their names
  (strings) or indices (integers). If `causal_ordering` is `NULL`
  (default), no causal ordering is assumed and all possible coalitions
  are allowed. No causal ordering is equivalent to a causal ordering
  with a single component that includes all features
  (`list(1:n_features)`) or groups (`list(1:n_groups)`) for feature-wise
  and group-wise Shapley values, respectively. For feature-wise Shapley
  values and `causal_ordering = list(c(1, 2), c(3, 4))`, the
  interpretation is that features 1 and 2 are the ancestors of features
  3 and 4, while features 3 and 4 are on the same level. Note: All
  features/groups must be included in `causal_ordering` without
  duplicates.

- sort_features_in_coalitions:

  Boolean. If `TRUE`, then the feature indices in the coalitions are
  sorted in increasing order. If `FALSE`, then the function maintains
  the order of features within each group given in `causal_ordering`.

## Value

List of vectors containing all coalitions that respects the causal
ordering.

## Author

Lars Henry Berge Olsen
