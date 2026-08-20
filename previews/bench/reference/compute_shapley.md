# Compute Shapley Values

Compute Shapley Values

## Usage

``` r
compute_shapley(internal, dt_vS)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

- dt_vS:

  The contribution matrix.

## Value

A `data.table` with Shapley values for each test observation.
