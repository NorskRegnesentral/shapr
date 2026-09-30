# Get table with all (exact) coalitions

Get table with all (exact) coalitions

## Usage

``` r
exact_coalition_table(
  m,
  max_fixed_coal_size = ceiling((m - 1)/2),
  dt_valid_causal_coalitions = NULL,
  weight_zero_m = 10^6
)
```

## Arguments

- m:

  Positive integer. Total number of features/groups.

- dt_valid_causal_coalitions:

  data.table. Only applicable for asymmetric Shapley value explanations,
  and is `NULL` for symmetric Shapley values. The data.table contains
  information about the coalitions that respects the causal ordering.

- weight_zero_m:

  Numeric. The value to use as a replacement for infinite coalition
  weights when doing numerical operations.
