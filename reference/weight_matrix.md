# Calculate Weighted Matrix

Calculate Weighted Matrix

## Usage

``` r
weight_matrix(X, normalize_W_weights = TRUE)
```

## Arguments

- X:

  data.table. Output from
  [`create_coalition_table()`](https://norskregnesentral.github.io/shapr/reference/create_coalition_table.md).

- normalize_W_weights:

  Logical. Whether to normalize the coalition weights to sum to 1 for
  increased numerical stability before solving the WLS (weighted least
  squares). Applies to all coalitions except coalitions `1` and `2^m`.

## Value

Numeric matrix. See
[`weight_matrix_cpp()`](https://norskregnesentral.github.io/shapr/reference/weight_matrix_cpp.md)
for more information.

## Author

Nikolai Sellereite, Martin Jullum
