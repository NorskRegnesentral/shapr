# Calculate Shapley weight

Calculate Shapley weight

## Usage

``` r
shapley_weights(m, N, n_components, weight_zero_m = 10^6)
```

## Arguments

- m:

  Positive integer. Total number of features/groups.

- N:

  Positive integer. The number of unique coalitions when sampling
  `n_components` features/feature groups, without replacement, from a
  sample space consisting of `m` different features/feature groups.

- n_components:

  Positive integer. Represents the number of features/feature groups you
  want to sample from a feature space consisting of `m` unique
  features/feature groups. Note that ` 0 < = n_components <= m`.

- weight_zero_m:

  Numeric. The value to use as a replacement for infinite coalition
  weights when doing numerical operations.

## Value

Numeric

## Author

Nikolai Sellereite
