# Get the steps for generating MC samples for coalitions following a causal ordering

Get the steps for generating MC samples for coalitions following a
causal ordering

## Usage

``` r
get_S_causal_steps(S, causal_ordering, confounding, as_string = FALSE)
```

## Arguments

- S:

  Integer matrix of dimension `n_coalitions_valid x m`, where
  `n_coalitions_valid` equals the total number of valid coalitions that
  respect the causal ordering given in `causal_ordering` and `m` equals
  the total number of features.

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

- confounding:

  Logical vector. Not applicable for (regular) non-causal or asymmetric
  explanations. `confounding` is a logical vector specifying whether
  confounding is assumed for each component in the `causal_ordering`. If
  `NULL` (default), no assumption about the confounding structure is
  made and `explain` computes asymmetric/symmetric conditional Shapley
  values, depending on `asymmetric`. If `confounding` is a single
  logical (`FALSE` or `TRUE`), the assumption is set globally for all
  components in the causal ordering. Otherwise, `confounding` must have
  the same length as `causal_ordering`, indicating the confounding
  assumption for each component. When `confounding` is specified,
  `explain` computes asymmetric/symmetric causal Shapley values,
  depending on `asymmetric`. The `approach` cannot be
  `regression_separate` or `regression_surrogate`, as the
  regression-based approaches are not applicable to the causal Shapley
  methodology.

- as_string:

  Boolean. If the returned object is to be a list of lists of integers
  or a list of vectors of strings.

## Value

Depends on the value of the parameter `as_string`. If a string, then
`results[j]` is a vector specifying the process of generating the
samples for coalition `j`. The length of `results[j]` is the number of
steps, and `results[j][i]` is a string of the form
`features_to_sample|features_to_condition_on`. If the
`features_to_condition_on` part is blank, then we are to sample from the
marginal distribution. For `as_string == FALSE`, then we rather return a
vector where `results[[j]][[i]]` contains the elements `Sbar` and `S`
representing the features to sample and condition on, respectively.

## Author

Lars Henry Berge Olsen
