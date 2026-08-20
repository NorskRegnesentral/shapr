# Compute the SAGE Value Function

Compute the SAGE Value Function

## Usage

``` r
compute_vS_loss(dt_vS, internal)
```

## Arguments

- dt_vS:

  Data.table with the contribution function `v(S)` estimates for each
  coalition.

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

## Value

A single-column numeric matrix with one row per coalition in `dt_vS`.

## Details

For each coalition (row of `dt_vS`), the value function used for the
SAGE values is the negative model loss `-loss_func(y_explain, v(S))`,
where `v(S)` is the vector of conditional expectations across the
explained observations.

## Author

Martin Jullum
