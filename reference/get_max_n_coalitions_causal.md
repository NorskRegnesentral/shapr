# Get the number of coalitions that respects the causal ordering

Get the number of coalitions that respects the causal ordering

## Usage

``` r
get_max_n_coalitions_causal(causal_ordering)
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

## Value

Integer. The (maximum) number of coalitions that respects the causal
ordering.

## Details

The function computes the number of coalitions that respects the causal
ordering by computing the number of coalitions in each partial causal
component and then summing these. We compute the number of coalitions in
the \\i\\th a partial causal component by \\2^n - 1\\, where \\n\\ is
the number of features in the \\i\\th partial causal component and we
subtract one as we do not want to include the situation where no
features in the \\i\\th partial causal component are present. In the
end, we add 1 for the empty coalition.

## Author

Lars Henry Berge Olsen
