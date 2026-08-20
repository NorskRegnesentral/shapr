# Convert feature names into feature indices

Functions that takes a `causal_ordering` specified using strings and
convert these strings to feature indices.

## Usage

``` r
convert_feature_name_to_idx(causal_ordering, labels, feat_group_txt)
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

- labels:

  Vector of strings containing (the order of) the feature names.

- feat_group_txt:

  String that is either "feature" or "group" based on if `shapr` is
  computing feature- or group-wise Shapley values

## Value

The `causal_ordering` list, but with feature indices (w.r.t. `labels`)
instead of feature names.

## Author

Lars Henry Berge Olsen
