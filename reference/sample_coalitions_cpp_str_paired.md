# We here return a vector of strings/characters, i.e., a CharacterVector, where each string is a space-separated list of integers.

We here return a vector of strings/characters, i.e., a CharacterVector,
where each string is a space-separated list of integers.

## Usage

``` r
sample_coalitions_cpp_str_paired(m, n_coalitions, paired_shap_sampling = TRUE)
```

## Arguments

- m:

  Positive integer. Total number of features/groups.

- n_coalitions:

  IntegerVector. The number of features to sample for each feature
  combination.

- paired_shap_sampling:

  Logical. Whether to do paired sampling of coalitions.
